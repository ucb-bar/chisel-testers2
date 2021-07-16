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
  override def getSimulator: Simulator = TreadleSimulator

  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "t-use-treadle",
      toAnnotationSeq = _ => Seq(TreadleBackendAnnotation),
      helpText = "direct tester to use Treadle backend"
    )
  )
}

private object TreadleSimulator extends Simulator {
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

    new TreadleContext(treadleTester, toplevel)
  }

  private def translateAnnotation(a: Annotation): Annotation = a match {
    case WriteVcdAnnotation => treadle.WriteVcdAnnotation
    case other              => other
  }

  private def toAnnos(state: CircuitState): AnnotationSeq =
    FirrtlCircuitAnnotation(state.circuit) +: state.annotations
}

private class TreadleContext(tester: TreadleTester, toplevel: TopmoduleInfo) extends SimulatorContext {
  override def sim: Simulator = TreadleSimulator

  require(toplevel.clocks.size <= 1, "Currently only single clock circuits are supported!")
  private def defaultClock = toplevel.clocks.headOption
  override def step(n: Int): Unit = {
    defaultClock match {
      case Some(value) =>
      case None        => throw NoClockException(tester.topName)
    }

    try {
      tester.step(n = n)
    } catch {
      case s: StopException =>
      // TODO: throw exception!
    }
  }

  override def peek(signal: String): BigInt = tester.peek(signal)

  override def poke(signal: String, value: BigInt): Unit = tester.poke(signal, value)

  override def finish(): Unit = tester.finish

  override def resetCoverage(): Unit = tester.resetCoverage()

  override def getCoverage(): List[(String, Long)] = tester.getCoverage()
}
