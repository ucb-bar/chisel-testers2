// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator

import firrtl._
import firrtl.annotations.NoTargetAnnotation
import firrtl.options.{HasShellOptions, ShellOption}

/** context for a running firrtl circuit simulation */
trait SimulatorContext {

  /** Returns a reference the [[Simulator]] that created this context. */
  def sim: Simulator

  /** Step the main clock `n` times. Throws a [[NoClockException]] if the circuit does not have a clock input. */
  def step(n: Int = 1): Unit

  /** Returns the latest value of an output or input port on the top-level module.
    * @note the simulator has to take care of recomputing signals after any change
    */
  def peek(signal: String): BigInt

  /** Change the value of an input port on the top-level module. */
  def poke(signal: String, value: BigInt): Unit

  /** Needs to be called after the context is no longer needed.
    * @note only after `finish` has been successfully returned is a potential waveform file guaranteed to exists on disk.
    * @note after `finish` no other functions besides `sim` or `getCoverage` may be called.
    */
  def finish(): Unit

  /** Returns the current value of the coverage counters.
    * @note only some simulators support calling `getCoverage` _before_ `finish`
    * @note for more information on the coverage format, please consult the documentation for [[chiseltest.coverage.TestCoverage]]
    */
  def getCoverage(): List[(String, Long)]

  /** Resets all coverage counters to zero.
    * @note Not supported by all simulators. Must result in a [[NotImplementedError]] if not supported.
    */
  def resetCoverage(): Unit
}

/** Throws by [[SimulatorContext.step]] if the circuit has no clock input */
case class NoClockException(toplevel: String)
    extends Exception(s"Circuit $toplevel has now clock and therefore cannot be stepped!")

/** a firrtl circuit simulator */
trait Simulator {
  def name: String

  /** is this simulator installed on the local machine? */
  def isAvailable: Boolean

  /** search the local computer for an installation of this simulator and print versions */
  def findVersions: Unit

  /** start a new simulation
    * @param state LoFirrtl circuit + annotations
    */
  def createContext(state: CircuitState): SimulatorContext
}

/** Defines a simulator backend that should be used. */
trait SimulatorAnnotation extends NoTargetAnnotation {
  def getSimulator: Simulator
}

trait WriteWaveformAnnotation extends NoTargetAnnotation {
  def format: String
}

case object WriteVcdAnnotation extends WriteWaveformAnnotation with HasShellOptions {
  override def format: String = "vcd"
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "t-write-vcd",
      toAnnotationSeq = _ => Seq(WriteVcdAnnotation),
      helpText = "writes vcd execution log"
    )
  )
}

case object WriteFstAnnotation extends WriteWaveformAnnotation with HasShellOptions {
  override def format: String = "fst"
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "t-write-fst",
      toAnnotationSeq = _ => Seq(WriteVcdAnnotation),
      helpText = "writes fst execution log"
    )
  )
}

case object WriteVpdAnnotation extends WriteWaveformAnnotation with HasShellOptions {
  override def format: String = "vpd"
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "t-write-vpd",
      toAnnotationSeq = _ => Seq(WriteVcdAnnotation),
      helpText = "writes vpd execution log"
    )
  )
}

/** contains some common code that is used by the various simulator backends */
private[chiseltest] object Simulator {
  def getWavformFormat(annos: AnnotationSeq): String = {
    val formats = annos.collect { case a: WriteWaveformAnnotation => a.format }.distinct
    assert(formats.size <= 1, s"Cannot select more than one waveform format! $formats")
    formats.headOption.getOrElse("")
  }

  def getSimulator(annos: AnnotationSeq, default: SimulatorAnnotation): Simulator =
    getSimulatorOptionalDefault(annos, Some(default))
  def getSimulator(annos: AnnotationSeq): Simulator = getSimulatorOptionalDefault(annos, None)
  private def getSimulatorOptionalDefault(annos: AnnotationSeq, default: Option[SimulatorAnnotation]): Simulator = {
    val simAnnos = annos.collect { case s: SimulatorAnnotation => s }.distinct
    if (simAnnos.isEmpty) {
      default match {
        case Some(value) => value.getSimulator
        case None        => throw new RuntimeException("No backend specified!")
      }
    }
    if (simAnnos.length > 1) {
      throw new RuntimeException(
        s"Multiple simulator backends were specified: ${simAnnos.map(_.getSimulator.name).mkString(", ")}"
      )
    }
    simAnnos.head.getSimulator
  }
}

/** Contains information about the top-level module in the circuit being simulated. */
private[chiseltest] case class TopmoduleInfo(
  name:    String,
  inputs:  Seq[(String, Int)],
  outputs: Seq[(String, Int)],
  clocks:  Seq[String]) {
  require(inputs.forall(_._2 > 0), s"Inputs need to be at least 1-bit!\n$inputs")
  require(outputs.forall(_._2 > 0), s"Outputs need to be at least 1-bit!\n$outputs")
}

private[chiseltest] object TopmoduleInfo {
  def apply(circuit: ir.Circuit): TopmoduleInfo = {
    val main = circuit.modules.find(_.name == circuit.main).get

    // extract ports
    // clock outputs are treated just like any other output
    def isClockIn(p: ir.Port): Boolean = p.tpe == ir.ClockType && p.direction == ir.Input
    val (clock, notClock) = main.ports.partition(isClockIn)
    val (in, out) = notClock.filterNot(p => bitWidth(p.tpe) == 0).partition(_.direction == ir.Input)

    new TopmoduleInfo(
      name = main.name,
      inputs = in.map(portNameAndWidth),
      outputs = (out).map(portNameAndWidth),
      clocks = clock.map(_.name)
    )
  }

  private def portNameAndWidth(p: ir.Port): (String, Int) = {
    require(
      p.tpe.isInstanceOf[ir.GroundType],
      s"Port ${p.serialize} is not of ground type! Please make sure to provide LowFirrtl to this API!"
    )
    p.name -> bitWidth(p.tpe).toInt
  }
}
