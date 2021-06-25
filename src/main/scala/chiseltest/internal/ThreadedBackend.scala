// SPDX-License-Identifier: Apache-2.0

package chiseltest.internal

import java.util.concurrent.{ConcurrentLinkedQueue, Semaphore}

import chiseltest.{Region, TemporalParadox, ThreadOrderDependentException}
import chisel3._

import scala.collection.mutable

case class ForkBuilder(name: Option[String], region: Option[Region], threads: Seq[AbstractTesterThread]) {
  def apply(runnable: => Unit): TesterThreadList = {
    new TesterThreadList(threads ++ Seq(Context().backend.doFork(() => runnable, name, region)))
  }

  def withRegion(newRegion: Region): ForkBuilder = {
    require(region.isEmpty)
    this.copy(region = Some(newRegion))
  }
  def withName(newName: String): ForkBuilder = {
    require(name.isEmpty)
    this.copy(name = Some(newName))
  }
}

/** Base trait for backends implementing concurrency by threading.
  *
  * Implements these BackendInterface methods:
  * - doFork
  * - doJoin
  *
  * Provides these methods for use by subclasses:
  * - doPoke, doPeek, which logs peek and poke actions for cross-thread-interaction checking
  * - newTimescope, closeTimescope: provides record-keeping for timescopes
  * - runThreads: runs all threads waiting on a set of clocks
  * - scheduler: called from within a test thread, suspends the current thread and runs the next one
  */
trait ThreadedBackend[T <: Module] extends BackendInterface {
  def dut: T

  // State for deadlock detection timeout
  val idleCycles = mutable.Map[Clock, Int]()
  val idleLimits = mutable.Map[Clock, Int](dut.clock -> 1000)

  override def setTimeout(signal: Clock, cycles: Int): Unit = {
    require(signal == dut.clock, "timeout currently only supports master clock")
    if (cycles == 0) {
      idleLimits.remove(signal)
    } else {
      idleLimits.put(signal, cycles)
    }
    idleCycles.remove(signal)
  }

  //
  // Variable references
  //
  val combinationalPaths: Map[Data, Set[Data]] // set of combinational Data inputs for each output

  //
  // Common variables
  //
  var currentTimestep: Int = 0 // current simulator timestep, in number of base clock cycles

  case class TimeRegion(timeslot: Int, region: Region) {
    def isBefore(other: TimeRegion): Boolean = {
      timeslot < other.timeslot || (timeslot == other.timeslot && region.isBefore(other.region))
    }
  }
  def currentTime: TimeRegion = TimeRegion(currentTimestep, schedulerState.currentRegion.get)

  //
  // Timescope data structures and utilities
  //

  sealed abstract class BaseTimescope {
    def threadOption: Option[TesterThread]
  }

  sealed trait HasOverridingPokes extends BaseTimescope {
    // List of timescopes overriding a signal, including ones closed / reverted on this timestep
    private[chiseltest] val overridingPokes = new mutable.HashMap[Data, mutable.ListBuffer[Timescope]]
    def closedTime: Option[TimeRegion]
  }
  sealed trait HasParent extends BaseTimescope {
    def parentTimescope: BaseTimescope
    def parentActionId:  Int
  }

  // Root timescope, cannot be modified, with no signals poked.
  class RootTimescope extends BaseTimescope with HasOverridingPokes {
    override def threadOption: Option[TesterThread] = None
    override def closedTime:   Option[TimeRegion] = None
  }

  var rootTimescope: Option[RootTimescope] = None // TODO unify with something else? Or replace w/ rootThread?

  // Timescope that is the root of a thread, contains no signals but can have a parent
  // TODO: should this be a trait of thread?
  case class ThreadRootTimescope(
    parentTimescope: BaseTimescope,
    openedTime:      TimeRegion,
    parentActionId:  Int,
    thread:          TesterThread)
      extends BaseTimescope
      with HasParent {
    override def threadOption = Some(thread)
  }

  case class PokeRecord(time: TimeRegion, actionId: Int, value: BigInt, trace: Throwable)

  class Timescope(
    val parentTimescope: BaseTimescope,
    val openedTime:      TimeRegion, // timestep this timescope was spawned on
    val parentActionId:  Int) // spawn time in actionId of parent timescope,
  // from parent's perspective all actions happen instantaneously in this timestep
      extends BaseTimescope
      with HasOverridingPokes
      with HasParent {
    var nextActionId: Int = 0 // actionId to be assigned to the next action
    var closedTime:   Option[TimeRegion] = None // timestep this timescope was closed on, if it is closed

    override def threadOption: Option[TesterThread] = parentTimescope.threadOption

    // All pokes on a signal in this timescope, ordered from first to last
    // TODO: can we get away with just first and most recent?
    private[chiseltest] val pokes =
      new mutable.HashMap[Data, mutable.ListBuffer[PokeRecord]] // Latest poke on each data
  }

  private[chiseltest] val pokes = new mutable.HashMap[Data, Timescope]

  object TimescopeUtils {
    // From the current timescope upwards, returns the nearest timescope poking the target signal
    // and the actionId to the child downwards
    def getNearestPoke(signal: Data, timescope: BaseTimescope, actionId: Int): (HasOverridingPokes, Int) =
      timescope match {
        case timescope: RootTimescope                                 => (timescope, actionId)
        case timescope: Timescope if timescope.pokes.contains(signal) => (timescope, actionId)
        case timescope: HasParent                                     => getNearestPoke(signal, timescope.parentTimescope, timescope.parentActionId)
      }

    /** Returns the linear path from startTimescope (as the first element, inclusive)
      * to destTimescope (as the last element, inclusive)
      * destActionId is the actionId in destTimescope to the next timescope (or the action of interest)
      */
    def getLinearPath(
      startTimescope: HasOverridingPokes,
      destTimescope:  BaseTimescope,
      destActionId:   Int
    ): Seq[(BaseTimescope, Int)] = {
      val prefix: Seq[(BaseTimescope, Int)] = destTimescope match {
        case _ if destTimescope == startTimescope => Seq()
        case destTimescope: HasParent =>
          getLinearPath(startTimescope, destTimescope.parentTimescope, destTimescope.parentActionId)
        case _ => throw new IllegalArgumentException("no path from startTimescope to destTimescope")
      }
      prefix :+ ((destTimescope, destActionId))
    }

    /** Returns the deepest common ancestor timescope from the first element in the linearPath to the last element in
      * the linearPath and the destination, and the actionIds to the next element for the linearPath, and destination.
      */
    def getCommonAncestor(
      linearPath:    Seq[(BaseTimescope, Int)],
      destTimescope: Timescope,
      destActionId:  Int
    ): (HasOverridingPokes, Int, Int) = {
      // TODO: clean up all the asInstanceOf
      val destinationLinearPath =
        getLinearPath(linearPath.head._1.asInstanceOf[HasOverridingPokes], destTimescope, destActionId)
      val commonPrefix = (linearPath.zip(destinationLinearPath)).takeWhile { case ((lpTimescope, _), (dTimescope, _)) =>
        lpTimescope == dTimescope
      }
      val commonAncestor = commonPrefix.last
      (commonAncestor._1._1.asInstanceOf[HasOverridingPokes], commonAncestor._1._2, commonAncestor._2._2)
    }

    /** Returns all the leaf-level overriding pokes, not include the argument timescope (if it has overriding pokes)
      */
    def getLeafOverridingPokes(timescope: HasOverridingPokes, signal: Data): Seq[Timescope] = {
      def innerOverridingPokes(timescope: Timescope): Seq[Timescope] = {
        timescope.overridingPokes.get(signal) match {
          case Some(overridingPokes) => overridingPokes.map(innerOverridingPokes).fold(Seq())(_ ++ _)
          case None                  => Seq(timescope)
        }
      }

      timescope.overridingPokes.get(signal) match {
        case Some(overridingPokes) => overridingPokes.map(innerOverridingPokes).fold(Seq())(_ ++ _)
        case None                  => Seq()
      }
    }
  }

  //
  // Threading checking data structures and utilities
  //

  case class PeekRecord(
    timescope: Timescope,
    time:      TimeRegion,
    actionId:  Int,
    deps:      Map[Data, Option[Timescope]],
    trace:     Throwable)

  // Active peeks on a signal, instantaneous on the current timestep
  // TODO: should this last until the next associated clock edge?
  private[chiseltest] val signalPeeks = new mutable.HashMap[Data, mutable.ListBuffer[PeekRecord]]

  /** Logs a poke operation for later checking.
    * Returns whether to execute it, based on priorities compared to other active pokes.
    */
  def doPoke(signal: Data, value: BigInt, trace: Throwable): Unit = {
    val timescope = currentThread.get.getTimescope
    // On the first poke, add a link from the previous timescope
    if (!timescope.pokes.contains(signal)) {

      /** Return if the timescope modified signal before childTimescope spawned
        * (or, if childTimescope considers timescope a parent wrt the signal)
        */
      def timescopeContainsThreadSignal(
        signal:         Data,
        timescope:      Timescope,
        childTimescope: ThreadRootTimescope
      ): Boolean = {
        timescope.pokes.get(signal) match {
          case None              => false
          case Some(pokeRecords) => pokeRecords.head.actionId < childTimescope.parentActionId
        }
      }

      def getContainingTimescope(
        signal:         Data,
        timescope:      BaseTimescope,
        childTimescope: BaseTimescope
      ): HasOverridingPokes = {
        (timescope, childTimescope) match {
          case (timescope: RootTimescope, _) => timescope
          case (timescope: Timescope, _: Timescope) if timescope.pokes.contains(signal) => timescope
          case (timescope: Timescope, childTimescope: ThreadRootTimescope)
              if timescopeContainsThreadSignal(signal, timescope, childTimescope) =>
            timescope
          // TODO: dedup above
          case (timescope: HasParent, _) => getContainingTimescope(signal, timescope.parentTimescope, timescope)
        }
      }

      getContainingTimescope(signal, timescope.parentTimescope, timescope).overridingPokes
        .getOrElseUpdate(signal, mutable.ListBuffer[Timescope]())
        .append(timescope)
    }

    // Update the timescope
    timescope.pokes
      .getOrElseUpdate(signal, mutable.ListBuffer[PokeRecord]())
      .append(PokeRecord(currentTime, timescope.nextActionId, value, trace))
    timescope.nextActionId += 1
    pokes.put(signal, timescope)
  }

  /** Logs a peek operation for later checking.
    */
  def doPeek(signal: Data, trace: Throwable): Unit = {
    val timescope = currentThread.get.getTimescope
    val deps = combinationalPaths
      .getOrElse(signal, Set(signal))
      .map { signal =>
        (signal, pokes.get(signal))
      }
      .toMap

    signalPeeks.getOrElseUpdate(signal, mutable.ListBuffer()) +=
      PeekRecord(timescope, currentTime, timescope.nextActionId, deps, trace)
    timescope.nextActionId += 1
  }

  /** Creates a new timescope in the current thread.
    */
  def newTimescope(): Timescope = {
    val newTimescope = currentThread.get.topTimescope match {
      case timescope: Timescope =>
        val newTimescope = new Timescope(timescope, currentTime, timescope.nextActionId)
        timescope.nextActionId += 1
        newTimescope
      case timescope => new Timescope(timescope, currentTime, 0)
    }
    currentThread.get.topTimescope = newTimescope
    newTimescope
  }

  /** Closes the specified timescope, returns a map of wires to values of any signals that need to be updated.
    */
  def closeTimescope(timescope: Timescope): Map[Data, Option[BigInt]] = {
    require(timescope eq currentThread.get.getTimescope)

    // Mark timescope as closed
    timescope.closedTime = Some(currentTime)
    currentThread.get.topTimescope = timescope.parentTimescope

    // Build a revert map of poked signals
    timescope.pokes.map { case (data, _) =>
      def getPreviousPoke(startingTimescope: BaseTimescope, signal: Data): Option[PokeRecord] = {
        // TODO make this less side-effect-like
        startingTimescope match {
          case _: RootTimescope =>
            pokes.remove(signal)
            None
          case startingTimescope: ThreadRootTimescope => getPreviousPoke(startingTimescope.parentTimescope, signal)
          case startingTimescope: Timescope =>
            startingTimescope.pokes.get(signal) match {
              case Some(pokeRecord) =>
                pokes.put(signal, startingTimescope)
                Some(pokeRecord.last)
              case None => getPreviousPoke(startingTimescope.parentTimescope, signal)
            }
        }
      }
      val previousPoke = getPreviousPoke(timescope.parentTimescope, data)
      (data, previousPoke.map(_.value))
    }.toMap
  }

  /** Starts a new timestep, checking if there were any conflicts on the previous timestep (and
    * throwing exceptions if there were).
    */
  def timestep(): Unit = {
    // Check peeks first, before timescope overridingPokes gets purged
    // These are checked by walking up the tree of timescopes, and ensuring the closest poke
    // has not been overridden.
    // Conflicting pokes will be detected by poke checking.
    signalPeeks.toSeq.foreach { case (peekSignal, peeks) =>
      // Check both the signal and combinational sources
      val upstreamSignals = combinationalPaths.getOrElse(peekSignal, Set()) + peekSignal
      // TODO: optimization of peeks within a thread
      peeks.foreach { peekRecord =>
        upstreamSignals.foreach { upstreamSignal =>
          val peekTimescope = peekRecord.timescope
          val (pokeTimescope, peekActionId) =
            TimescopeUtils.getNearestPoke(upstreamSignal, peekTimescope, peekRecord.actionId)

          //
          // Bulk of the check logic is here
          //
          // Check override signals
          val pokeToPeekPath = TimescopeUtils.getLinearPath(pokeTimescope, peekTimescope, peekActionId)
          val overridingPokes = TimescopeUtils.getLeafOverridingPokes(pokeTimescope, upstreamSignal)
          overridingPokes.foreach { overridingTimescope =>
            // Use a dummy actionId for the poke, because it should never be used (overridingTimescope should not be on the path to the peek)
            // 0 is used because where it matter, it will fail.
            val (branchTimescope, branchPeekActionId, branchPokeActionId) =
              TimescopeUtils.getCommonAncestor(pokeToPeekPath, overridingTimescope, 0)
            if (overridingTimescope.threadOption == peekTimescope.threadOption) { // override in peek timescope
              // Pokes within the same thread as the peek are always fine
            } else if (overridingTimescope.pokes(upstreamSignal).last.time.region.isBefore(peekRecord.time.region)) {
              // Peek-after-poke dependencies in different regions are always allowed
              // TODO: should this check ALL pokes, or just the last effective one? This currently only checks the last effective one
            } else if (branchTimescope.threadOption == peekTimescope.threadOption) { // branched to another thread, from peek thread
              if (branchPeekActionId > branchPokeActionId) { // thread must have spawned after the peek
                throw new ThreadOrderDependentException(
                  s"$upstreamSignal -> $peekSignal: Poking thread spawned before peek"
                )
              }
            } else { // branch point was in a parent thread
              if (overridingTimescope.threadOption == branchTimescope.threadOption) { // ... remaining in the same thread as the parent
                if (branchPeekActionId > branchPokeActionId) { // thread must have spawned after the peek
                  throw new ThreadOrderDependentException(
                    s"$upstreamSignal -> $peekSignal: Poking thread spawned before peek"
                  )
                }
              } else { // ... into another thread
                throw new ThreadOrderDependentException(
                  s"$upstreamSignal -> $peekSignal: Divergent poking / peeking threads"
                )
              }
            }
          }

          // If the poke is from another (parent) thread, also check that it has not changed since the immediate
          // child spawned.
          if (pokeTimescope.threadOption != peekTimescope.threadOption) {
            pokeTimescope.closedTime match {
              case Some(closedTime)
                  if closedTime.isBefore(currentTime) => // signal cannot be changed by timescope revert
                throw new ThreadOrderDependentException(s"$upstreamSignal -> $peekSignal: Timescope revert")
              case _ => // revert is fine on the current timestep:
              // if the peeking thread was just spawned, is should be encapsulated and run immediately
              // if the peeking thread was spawned before, it would have run before the parent run (and closed the timescope)
            }
            pokeTimescope match {
              case timescope: Timescope =>
                if (timescope.pokes(upstreamSignal).last.actionId > peekActionId) {
                  throw new ThreadOrderDependentException(
                    s"$upstreamSignal -> $peekSignal: Timescope signal changed, poked after peek"
                  )
                }
              case _: RootTimescope => // catch issues with override checks below
            }
          }
        }
      }
    }

    // Clear peeks
    signalPeeks.clear()

    // Timescopes are valid if there is a linear chain from root to latest
    // Takes a signal and returns the leaf timescope, throwing an error if the chain is nonlinear
    def processTimescope(signal: Data, timescope: HasOverridingPokes): Option[Timescope] = {
      val overridingTimescopes = timescope.overridingPokes.getOrElse(signal, mutable.ListBuffer())
      if (overridingTimescopes.exists(_.closedTime.isDefined)) { // remove closed child timescopes
        if (
          overridingTimescopes
            .exists(timescope => timescope.closedTime.isEmpty && (timescope.openedTime.isBefore(currentTime)))
        ) {
          throw new ThreadOrderDependentException("Mix of ending timescopes and old timescopes")
        }
        val (endingPokes, nonEndingPokes) = overridingTimescopes.partition(_.closedTime.isDefined)
        endingPokes.foreach { processTimescope(signal, _) } // Recursively check child closing timescopes
        overridingTimescopes.clear()
        overridingTimescopes ++= nonEndingPokes
      }
      if (timescope.closedTime.isDefined) { // if this timescope is closed, ensure there are no children
        if (overridingTimescopes.nonEmpty) {
          throw new ThreadOrderDependentException(s"Non-enclosed timescopes detected. Did you forget to call join after fork?")
        }
      }
      if (overridingTimescopes.length > 1) { // multiple overlapping pokes, is an error
        // TODO: better stack trace element detection, chain reporting
        val pokeTraces = overridingTimescopes.map { _.pokes(signal).last.trace.getStackTrace()(3) }
        throw new ThreadOrderDependentException(s"Overlapping pokes from $pokeTraces")
      } else if (overridingTimescopes.length == 1) { // just one poke, report the furthest down that chain
        processTimescope(signal, overridingTimescopes.head)
      } else { // all children are closed, this is the latest one in this chain
        timescope.overridingPokes.remove(signal)
        timescope match {
          case timescope: Timescope     => Some(timescope)
          case _:         RootTimescope => None
        }
      }
    }

    //noinspection ScalaUnusedSymbol
    // Check that there is a clean poke ordering, and build a map of pokes Data -> Timescope
    // TODO: structurally nasty =(, and currently pokeTimescopes is un-used
    val pokeTimescopes = rootTimescope.get.overridingPokes.toMap.map { case (signal, _) =>
      (signal, processTimescope(signal, rootTimescope.get))
    }.collect { case (signal, Some(lastTimescope)) =>
      (signal, lastTimescope)
    }
    // TODO: check that lastTimescope signal is actually the one effective?
  }

  // Used to propagate exceptions from this thread back up to the main (testdriver) thread.
  // Once something is enqueued, this thread should not continue running.
  protected val interruptedException = new ConcurrentLinkedQueue[Throwable]()

  protected class TesterThread(
    runnable:        () => Unit,
    openedTime:      TimeRegion,
    parentTimescope: BaseTimescope,
    parentActionId:  Int,
    val region:      Region,
    val trace:       Option[(TesterThread, Throwable)])
      extends AbstractTesterThread {
    val level: Int = parentTimescope.threadOption match {
      case Some(parentThread) => parentThread.level + 1
      case None               => 0
    }
    val waiting = new Semaphore(0)
    var done: Boolean = false

    // Scheduling information
    var joinedOn:      Set[TesterThread] = Set()
    var joinPostClock: Option[Clock] = None
    var clockedOn:     Option[Clock] = None

    // TODO: perhaps accessors eg pushTimescope, popTimescope?
    protected val bottomTimescope = ThreadRootTimescope(parentTimescope, openedTime, parentActionId, this)
    var topTimescope: BaseTimescope = bottomTimescope // Currently open timescope in this thread
    def getTimescope: Timescope = {
      require(topTimescope.asInstanceOf[Timescope].closedTime.isEmpty)
      topTimescope.asInstanceOf[Timescope]
    }

    //noinspection ConvertExpressionToSAM
    //TODO: code analysis suggests "Convert expression to Single Abstract Method", will that work?
    val thread = new Thread(new Runnable {
      def run() {
        try {
          waiting.acquire()

          // TODO: maybe want to dedup w/ tester/package.timescope { ... }
          Context().backend.doTimescope(() => runnable())

          require(bottomTimescope == topTimescope) // ensure timescopes unrolled properly
          done = true
          threadFinished(TesterThread.this)
          scheduler()
        } catch {
          case _: InterruptedException =>
          // Currently used as a signal to kill the thread without doing cleanup
          // (test driver may be left in an inconsistent state, and the test should not continue)
          // Explicitly don't invoke the scheduler, just end the thread.
          case e @ (_: Exception | _: Error) =>
            interruptedException.offer(e)
            scheduler()
        }
      }
    })
  }

  protected var currentThread: Option[TesterThread] = None
  protected val driverSemaphore = new Semaphore(0) // blocks the driver thread while tests are running

  // List of threads in order they are run
  private[chiseltest] val allThreads = new mutable.ArrayBuffer[TesterThread]

  // TODO make this a class and Option[SchedulerState] var that contains currentThread and driverSemaphore?
  object schedulerState { // temporary scheduler information not persisting between runThreads invocations
    private[chiseltest] var currentRegion:      Option[Region] = None
    private[chiseltest] var currentThreadIndex: Int = 0
    // List of clocks that have stepped (run threads blocked on these clocks)
    private[chiseltest] var clocks = mutable.Set[Clock]()
  }

  /** Runs the specified threads, blocking this thread while those are running.
    * Newly formed threads or unblocked join threads will also run.
    * Returns a list of threads run (either passed in, newly forked, or joined) that are waiting
    * on a clock edge.
    *
    * TODO: uses shared schedule state, as an optimization (so that control doesn't need to return
    * to the driver thread between runs. But control still needs to return to the calling thread
    * between timesteps.
    * TODO: this provides a separation which isolates the threading infrastructure from clock
    * control, but there are other structures which accomplish the same thing.
    */
  protected def runThreads(clocks: Set[Clock]): Unit = {
    // TODO validate and order incoming thread order
    require(schedulerState.currentRegion.isEmpty)
    require(schedulerState.currentThreadIndex == 0)
    require(schedulerState.clocks.isEmpty)

    // For joinAndStep thread that have joined, propagate the waiting clock
    for (thread <- allThreads) {
      if (thread.joinedOn.isEmpty && thread.joinPostClock.isDefined) {
        require(thread.clockedOn.isEmpty)
        thread.clockedOn = thread.joinPostClock
        thread.joinPostClock = None
      }
    }

    currentTimestep += 1
    schedulerState.clocks ++= clocks

    for (region <- Region.allRegions) {
      schedulerState.currentRegion = Some(region)
      schedulerState.currentThreadIndex = 0

      scheduler()
      driverSemaphore.acquire()

      if (!interruptedException.isEmpty) {
        throw interruptedException.poll()
      }

      require(schedulerState.currentRegion.get == region)
      require(schedulerState.currentThreadIndex == allThreads.size)

      timestep()
    }

    schedulerState.currentRegion = None
    schedulerState.currentThreadIndex = 0
    schedulerState.clocks.clear()
  }

  /** Invokes the thread scheduler, which should be done anytime a thread needs to pass time.
    * Prior to this call: caller should add itself to the blocked / joined threads list
    * (unless terminating).
    * After this call: caller should block on its semaphore (unless terminating). currentThread
    * will no longer be valid.
    *
    * Unblocks the next thread to be run, possibly also also stepping time via advanceTime().
    * When there are no more active threads, unblocks the driver thread via driverSemaphore.
    */
  protected def scheduler() {
    def threadCanRun(thread: TesterThread): Boolean = {
      val blockedByJoin = thread.joinedOn match {
        case joinedOn if joinedOn.isEmpty => false
        case joinedOn if allThreads.toSet.intersect(joinedOn) == Set() =>
          thread.joinedOn = Set(); false
        case _ => true
      }
      val blockedByJoinStep = thread.joinPostClock match {
        case None    => false
        case Some(_) => true
      }
      val blockedByClock = thread.clockedOn match {
        case None        => false
        case Some(clock) => !schedulerState.clocks.contains(clock)
      }
      val blockedByRegion = thread.region != schedulerState.currentRegion.get
      !blockedByJoin && !blockedByJoinStep && !blockedByClock && !blockedByRegion
    }

    while (
      schedulerState.currentThreadIndex < allThreads.size &&
      !threadCanRun(allThreads(schedulerState.currentThreadIndex))
    ) {
      schedulerState.currentThreadIndex += 1
    }

    if (!interruptedException.isEmpty || schedulerState.currentThreadIndex >= allThreads.size) {
      currentThread = None
      driverSemaphore.release()
    } else {
      val nextThread = allThreads(schedulerState.currentThreadIndex)
      nextThread.clockedOn = None
      currentThread = Some(nextThread)
      nextThread.waiting.release()
    }
  }

  /** Called on thread completion to remove this thread from the running list.
    * Does not terminate the thread, does not schedule the next thread.
    */
  protected def threadFinished(thread: TesterThread): Unit = {
    allThreads -= thread
  }

  def doFork(runnable: () => Unit, name: Option[String], region: Option[Region]): TesterThread = {
    val timescope = currentThread.get.getTimescope
    val thisThread = currentThread.get // locally save this thread
    val newRegion = region.getOrElse(thisThread.region)
    if (newRegion.isBefore(thisThread.region)) {
      throw new TemporalParadox("Cannot spawn a thread at an earlier region")
    }
    val newThread = new TesterThread(
      runnable,
      currentTime,
      timescope,
      timescope.nextActionId,
      newRegion,
      Some(thisThread, new Throwable)
    )
    timescope.nextActionId += 1

    // schedule the new thread to run immediately, then return to this thread
    allThreads.insert(schedulerState.currentThreadIndex, newThread)
    newThread.thread.start()
    scheduler()
    thisThread.waiting.acquire()
    newThread
  }

  def doJoin(threads: Seq[AbstractTesterThread], stepAfter: Option[Clock]): Unit = {
    val thisThread = currentThread.get
    // TODO can this be made more typesafe?
    val joinThreadsTyped = threads.map(_.asInstanceOf[TesterThread])
    for (joinThread <- joinThreadsTyped) {
      // Ensure joined threads execute before this thread if there is no post-join step
      if ((joinThread.region.isAfter(thisThread.region)) && stepAfter.isEmpty) {
        throw new TemporalParadox("Cannot join (without step) on thread that would end in a later region")
      }
      if (
        joinThread.region == thisThread.region
        && allThreads.indexOf(joinThread) > allThreads.indexOf(thisThread)
        && stepAfter.isEmpty
      ) {
        throw new TemporalParadox("Cannot join (without step) on thread that would execute after this thread")
      }
    }

    thisThread.joinedOn = joinThreadsTyped.toSet
    thisThread.joinPostClock = stepAfter
    thisThread.clockedOn = None
    scheduler()
    thisThread.waiting.acquire()
  }

  override def getParentTraceElements: Seq[StackTraceElement] = {
    def processThread(thread: TesterThread): Seq[StackTraceElement] = thread.trace match {
      case Some((parent, trace)) => processThread(parent) ++ trace.getStackTrace
      case None                  => Seq()
    }
    processThread(currentThread.get)
  }
}
