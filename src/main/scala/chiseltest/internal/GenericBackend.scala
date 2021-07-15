// SPDX-License-Identifier: Apache-2.0

package chiseltest.internal

import chiseltest.{ClockResolutionException, Region, TimeoutException}
import chisel3._
import chiseltest.coverage.{Coverage, TestCoverage}
import chiseltest.simulator.SimulatorContext
import firrtl.AnnotationSeq

import scala.collection.mutable

/** Chiseltest threaded backend using the generic SimulatorContext abstraction from [[chiseltest.simulator]] */
class GenericBackend[T <: Module](
  val dut:                T,
  val dataNames:          Map[Data, String],
  val combinationalPaths: Map[Data, Set[Data]],
  tester:                 SimulatorContext,
  coverageAnnotations:    AnnotationSeq)
    extends BackendInstance[T]
    with ThreadedBackend[T] {

  //
  // Debug utility functions
  //
  val verbose: Boolean = false // hard-coded debug flag
  def debugLog(str: => String): Unit = {
    if (verbose) println(str)
  }

  protected def resolveName(signal: Data): String = { // TODO: unify w/ dataNames?
    dataNames.getOrElse(signal, signal.toString)
  }

  //
  // Circuit introspection functionality
  //
  override def getSourceClocks(signal: Data): Set[Clock] = {
    throw new ClockResolutionException("ICR not available on chisel-testers2 / firrtl master")
  }

  override def getSinkClocks(signal: Data): Set[Clock] = {
    throw new ClockResolutionException("ICR not available on chisel-testers2 / firrtl master")
  }

  //
  // Everything else
  //

  def getModule: T = dut

  override def pokeClock(signal: Clock, value: Boolean): Unit = {
    // TODO: check thread ordering
    val intValue = if (value) 1 else 0
    tester.poke(dataNames(signal), intValue)
    debugLog(s"${resolveName(signal)} <- $intValue")
  }

  override def peekClock(signal: Clock): Boolean = {
    doPeek(signal, new Throwable)
    val a = tester.peek(dataNames(signal))
    debugLog(s"${resolveName(signal)} -> $a")
    a > 0
  }

  override def pokeBits(signal: Data, value: BigInt): Unit = {
    doPoke(signal, value, new Throwable)
    if (tester.peek(dataNames(signal)) != value) {
      idleCycles.clear()
    }
    tester.poke(dataNames(signal), value)
    debugLog(s"${resolveName(signal)} <- $value")
  }

  override def peekBits(signal: Data, stale: Boolean): BigInt = {
    require(!stale, "Stale peek not yet implemented")

    doPeek(signal, new Throwable)
    val a = tester.peek(dataNames(signal))
    debugLog(s"${resolveName(signal)} -> $a")
    a
  }

  override def expectBits(
    signal:  Data,
    value:   BigInt,
    message: Option[String],
    decode:  Option[BigInt => String],
    stale:   Boolean
  ): Unit = {
    require(!stale, "Stale peek not yet implemented")

    debugLog(s"${resolveName(signal)} ?> $value")
    Context().env.testerExpect(value, peekBits(signal, stale), resolveName(signal), message, decode)
  }

  protected val clockCounter: mutable.HashMap[Clock, Int] = mutable.HashMap()
  protected def getClockCycle(clk: Clock): Int = {
    clockCounter.getOrElse(clk, 0)
  }
  protected def getClock(clk: Clock): Boolean = tester.peek(dataNames(clk)).toInt match {
    case 0 => false
    case 1 => true
  }

  protected val lastClockValue: mutable.HashMap[Clock, Boolean] = mutable.HashMap()

  override def doTimescope(contents: () => Unit): Unit = {
    val createdTimescope = newTimescope()

    contents()

    closeTimescope(createdTimescope).foreach { case (data, valueOption) =>
      valueOption match {
        case Some(value) =>
          if (tester.peek(dataNames(data)) != value) {
            idleCycles.clear()
          }
          tester.poke(dataNames(data), value)
          debugLog(s"${resolveName(data)} <- (revert) $value")
        case None =>
          idleCycles.clear()
          tester.poke(dataNames(data), 0) // TODO: randomize or 4-state sim
          debugLog(s"${resolveName(data)} <- (revert) DC")
      }
    }
  }

  override def step(signal: Clock, cycles: Int): Unit = {
    // TODO: maybe a fast condition for when threading is not in use?
    for (_ <- 0 until cycles) {
      // If a new clock, record the current value so change detection is instantaneous
      if (signal != dut.clock && !lastClockValue.contains(signal)) {
        lastClockValue.put(signal, getClock(signal))
      }

      val thisThread = currentThread.get
      thisThread.clockedOn = Some(signal)
      schedulerState.currentThreadIndex += 1
      scheduler()
      thisThread.waiting.acquire()
    }
  }

  private def clockName = dut.clock.name

  override def run(testFn: T => Unit): AnnotationSeq = {
    rootTimescope = Some(new RootTimescope)
    val mainThread = new TesterThread(
      () => {
        tester.poke("reset", 1)
        tester.step(List(clockName), 1)
        tester.poke("reset", 0)

        testFn(dut)
      },
      TimeRegion(0, Region.default),
      rootTimescope.get,
      0,
      Region.default,
      None
    )
    mainThread.thread.start()
    require(allThreads.isEmpty)
    allThreads += mainThread

    try {
      while (!mainThread.done) { // iterate timesteps
        clockCounter.put(dut.clock, getClockCycle(dut.clock) + 1)

        debugLog(s"clock step")

        // TODO: allow dependent clocks to step based on test stimulus generator
        // TODO: remove multiple invocations of getClock
        // Unblock threads waiting on dependent clock
        val steppedClocks = Seq(dut.clock) ++ lastClockValue.collect {
          case (clock, lastValue) if getClock(clock) != lastValue && getClock(clock) => clock
        }
        steppedClocks.foreach { clock =>
          clockCounter.put(dut.clock, getClockCycle(clock) + 1) // TODO: ignores cycles before a clock was stepped on
        }
        lastClockValue.foreach { case (clock, _) =>
          lastClockValue.put(clock, getClock(clock))
        }

        runThreads(steppedClocks.toSet)
        Context().env.checkpoint()

        idleLimits.foreach { case (clock, limit) =>
          idleCycles.put(clock, idleCycles.getOrElse(clock, -1) + 1)
          if (idleCycles(clock) >= limit) {
            throw new TimeoutException(s"timeout on $clock at $limit idle cycles")
          }
        }

        tester.step(List(clockName), 1)
      }
    } finally {
      rootTimescope = None

      for (thread <- allThreads.clone()) {
        // Kill the threads using an InterruptedException
        if (thread.thread.isAlive) {
          thread.thread.interrupt()
        }
      }

      tester.finish() // needed to dump VCDs
    }
    generateTestCoverageAnnotation() +: coverageAnnotations
  }

  /** Generates an annotation containing the map from coverage point names to coverage counts. */
  private def generateTestCoverageAnnotation(): TestCoverage = {
    TestCoverage(tester.getCoverage())
  }
}
