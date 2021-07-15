// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator

import firrtl.annotations.Annotation
import firrtl.options.{HasShellOptions, ShellOption}
import firrtl.stage.FirrtlCircuitAnnotation
import firrtl.{AnnotationSeq, CircuitState}
import treadle.{ClockInfoAnnotation, TreadleTester, TreadleTesterAnnotation}
import treadle.executable.{ClockInfo, StopException}
import treadle.stage.TreadleTesterPhase

case object TreadleBackendAnnotation extends SimulatorAnnotation with HasShellOptions {
  override def getSimulator = TreadleSimulator

  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "t-use-treadle",
      toAnnotationSeq = _ => Seq(TreadleBackendAnnotation),
      helpText = "direct tester to use Treadle backend"
    )
  )
}

object TreadleSimulator extends Simulator {
  override def name:        String = "treadle"
  override def isAvailable: Boolean = true
  override def findVersions: Unit = {
    println("treadle is available")
    println(s"version: ${treadle.BuildInfo.version}")
  }

  /** start a new simulation
    *
    * @param state LoFirrtl circuit + annotations
    */
  override def createContext(state: CircuitState): SimulatorContext = {
    // we need to annotate clocks for treadle to recognize them
    val toplevel = TopmoduleInfo(state.circuit)
    val clockAnno = ClockInfoAnnotation(toplevel.clocks.map(name => ClockInfo(name)))

    val annos = clockAnno +: toAnnos(state).map(translateAnnotation)

    val treadleState = (new TreadleTesterPhase).transform(annos)

    val treadleTester = treadleState.collectFirst { case TreadleTesterAnnotation(t) => t }.getOrElse(
      throw new Exception(
        s"TreadleTesterPhase could not build a treadle tester from these annotations" +
          treadleState.mkString("Annotations:\n", "\n  ", "")
      )
    )

    new TreadleContext(treadleTester)
  }

  private def translateAnnotation(a: Annotation): Annotation = a match {
    case WriteVcdAnnotation => treadle.WriteVcdAnnotation
    case other              => other
  }

  private def toAnnos(state: CircuitState): AnnotationSeq =
    FirrtlCircuitAnnotation(state.circuit) +: state.annotations
}

private class TreadleContext(tester: TreadleTester) extends SimulatorContext {
  override def sim: Simulator = TreadleSimulator

  require(tester.clockInfoList.size <= 1, "Currently only single clock circuits are supported!")
  private def defaultClock = tester.clockInfoList.headOption.map(_.name)
  override def step(clocks: List[String], n: Int): Unit = {
    defaultClock match {
      case Some(value) => require(clocks.isEmpty || clocks == List(value))
      case None        => throw new RuntimeException(s"Circuit has no clock, cannot be stepped!")
    }

    try {
      tester.step(n = n)
      None
    } catch {
      case s: StopException =>
        val exitCode = 1 // TODO!
        Some(SimulatorResults(exitCode))
    }
  }

  override def peek(signal: String): BigInt = tester.peek(signal)

  override def poke(signal: String, value: BigInt): Unit = tester.poke(signal, value)

  override def peekMemory(memory: String, index: Long): BigInt = {
    tester.peekMemory(memory, index.toInt)
  }

  override def pokeMemory(memory: String, index: Long, value: BigInt): Unit = {
    tester.pokeMemory(memory, index.toInt, value)
  }

  override def finish(): SimulatorResults = {
    tester.finish
    SimulatorResults(0)
  }

  override def resetCoverage(): Unit = tester.resetCoverage()

  override def getCoverage(): List[(String, Long)] = tester.getCoverage()
}
