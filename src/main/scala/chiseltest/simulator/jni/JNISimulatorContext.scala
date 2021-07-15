package chiseltest.simulator.jni

import chiseltest.simulator.ipc.TestApplicationException
import chiseltest.simulator.{Simulator, SimulatorContext, SimulatorResults, TopmoduleInfo, VerilatorCoverage}
import firrtl.AnnotationSeq
import logger.LazyLogging

import java.io.File
import scala.collection.immutable.ListMap
import scala.collection.mutable

/** This context works with a simulation binary that communicates through shared memory.
  * @param cmd command to launch the simulation binary
  * @param toplevel information about the interface exposed by the module at the top of the RTL hierarchy
  * @param sim simulator that generated the binary
  */
private[chiseltest] class JNISimulatorContext(
  lib:              os.Path,
  toplevel:         TopmoduleInfo,
  coverageAnnos:    AnnotationSeq,
  override val sim: Simulator)
    extends SimulatorContext
    with LazyLogging {
  require(toplevel.clocks.size == 1, "currently this interface only works with exactly one clock")
  (toplevel.inputs ++ toplevel.outputs).foreach { case (name, width) =>
    require(width <= 64, s"$width-bit I/O $name is not supported!")
  }

  def int(x: Int):  BigInt = (BigInt(x >>> 1) << 1) | BigInt(x & 1)
  def int(x: Long): BigInt = (BigInt(x >>> 1) << 1) | BigInt(x & 1)

  private var isStale = false
  private val _signalMap = mutable.HashMap[String, Int]()
  private val _logs = mutable.ArrayBuffer[String]()

  // load shared library
  private val so = TesterSharedLib(lib, _logs)

  private def update(): Unit = {
    so.update()
    isStale = false
  }

  private def takeStep(): Unit = {
    so.step()
  }

  private def getId(path: String): Int = {
    so.getid(path)
  }

  private def start(): Unit = {
    so.start()
    isRunning = true
  }

  override def poke(signal: String, value: BigInt): Unit = {
    val id = _signalMap.getOrElseUpdate(signal, getId(signal))
    if (id >= 0) {
      so.poke(id, value.toLong)
      isStale = true
    } else {
      logger.info(s"Can't find $signal in the emulator...")
    }
  }

  override def peek(signal: String): BigInt = {
    if (isStale) { update() }
    val id = _signalMap.getOrElseUpdate(signal, getId(signal))
    val r = if (id >= 0) {
      Some(so.peek(id))
    } else {
      logger.info(s"Can't find $signal in the emulator...")
      None
    }
    r.getOrElse(throw new RuntimeException(s"Could not find signal $signal"))
  }

  private def defaultClock = toplevel.clocks.headOption
  override def step(clocks: List[String], n: Int): Unit = {
    defaultClock match {
      case Some(value) => require(clocks.isEmpty || clocks == List(value))
      case None        => throw new RuntimeException(s"Circuit has no clock, cannot be stepped!")
    }
    try {
      update()
      (0 until n).foreach(_ => takeStep())
      None
    } catch {
      case TestApplicationException(exit, msg) =>
        Some(SimulatorResults(exit))
    }
  }

  private var isRunning = false
  override def finish(): SimulatorResults = {
    so.finish()
    isRunning = false
    // TODO: exit code?
    SimulatorResults(0)
  }

  // Once everything has been prepared, we can start the communications.
  start()

  override def peekMemory(memory: String, index: Long) = {
    throw new NotImplementedError("peekMemory")
  }

  override def pokeMemory(memory: String, index: Long, value: BigInt): Unit = {
    throw new NotImplementedError("pokeMemory")
  }

  private val coverageFile = lib / os.up / "coverage.dat"
  override def getCoverage(): List[(String, Long)] = {
    so.writeCoverage(coverageFile.toString())
    assert(os.exists(coverageFile), s"Could not find `$coverageFile` file!")
    VerilatorCoverage.loadCoverage(coverageAnnos, coverageFile)
  }

  override def resetCoverage(): Unit = {
    so.resetCoverage()
  }
}

class TesterSharedLib(libPath: os.Path) {
  try {
    System.load(libPath.toString())
  } catch {
    case e: Throwable =>
      println(s"Failed to load $libPath: " + e.toString)
      throw e
  }

  private val state: Long = 0

  @native private def sim_init(): Unit
  @native def start():            Unit
  @native def step():             Unit
  @native def update():           Unit
  @native def poke(id:    Int, value: Long): Unit
  @native def peek(id:    Int): Long
  @native def getid(path: String): Int
  @native def getchk(id:  Int):    Int
  @native def finish(): Unit
  @native def writeCoverage(filename: String): Unit
  @native def resetCoverage(): Unit

  //println(s"State before: $state")
  sim_init()
  //println(s"State after: $state")
}

private[chiseltest] object TesterSharedLib {
  def apply(lib: os.Path, logs: mutable.ArrayBuffer[String]): TesterSharedLib = {
    require(os.exists(lib), s"$lib doesn't exist")
    new TesterSharedLib(lib)
  }
}
