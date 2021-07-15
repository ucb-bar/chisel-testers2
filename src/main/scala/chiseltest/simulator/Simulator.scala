// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator

import firrtl._
import firrtl.annotations.NoTargetAnnotation
import firrtl.options.{HasShellOptions, ShellOption}

/** context for a running firrtl circuit simulation */
trait SimulatorContext {
  def sim: Simulator
  // TODO: maybe turn simulator results into exception?
  //       a poke could also trigger a stop statement...
  def step(clock:        List[String] = List(), n: Int = 1): Unit
  def peek(signal:       String): BigInt
  def poke(signal:       String, value: BigInt): Unit
  def peekMemory(memory: String, index: Long): BigInt
  def pokeMemory(memory: String, index: Long, value: BigInt): Unit
  def finish(): SimulatorResults
  // for possible optimizations
  def peekLong(signal:       String): Long = peek(signal).toLong
  def poke(signal:           String, value: Long): Unit = poke(signal, BigInt(value))
  def peekMemoryLong(memory: String, index: Long): Long = peekMemory(memory, index).toLong
  def pokeMemory(memory:     String, index: Long, value: Long): Unit = pokeMemory(memory, index, BigInt(value))
  // for fuzzing
  def getCoverage():   List[(String, Long)]
  def resetCoverage(): Unit
}

case class SimulatorResults(exitCode: Int)

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

case object WriteVcdAnnotation extends NoTargetAnnotation with HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "t-write-vcd",
      toAnnotationSeq = _ => Seq(WriteVcdAnnotation),
      helpText = "writes vcd execution log, this option may be moved into firrtl in the future"
    )
  )
}

/** contains some common code that is used by the various simulator backends */
object Simulator {
  def getWavformFormat(annos: AnnotationSeq): String = {
    val vcd = annos.contains(WriteVcdAnnotation)
    if (vcd) { "vcd" }
    else { "" }
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
case class TopmoduleInfo(name: String, inputs: Seq[(String, Int)], outputs: Seq[(String, Int)], clocks: Seq[String]) {
  require(inputs.forall(_._2 > 0), s"Inputs need to be at least 1-bit!\n$inputs")
  require(outputs.forall(_._2 > 0), s"Outputs need to be at least 1-bit!\n$outputs")
}

object TopmoduleInfo {
  def apply(circuit: ir.Circuit): TopmoduleInfo = {
    val main = circuit.modules.find(_.name == circuit.main).get

    // extract ports
    val (clock, notClock) = main.ports.partition(_.tpe == ir.ClockType)
    val (in, out) = notClock.filterNot(p => bitWidth(p.tpe) == 0).partition(_.direction == ir.Input)

    new TopmoduleInfo(
      name = main.name,
      inputs = in.map(portNameAndWidth),
      outputs = out.map(portNameAndWidth),
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
