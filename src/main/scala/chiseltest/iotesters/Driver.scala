// SPDX-License-Identifier: Apache-2.0

package chiseltest.iotesters

import chisel3.{ChiselExecutionFailure => _, ChiselExecutionSuccess => _, _}
import chiseltest.internal.Compiler
import chiseltest.simulator._
import firrtl.AnnotationSeq
import firrtl.annotations.Annotation
import firrtl.options.TargetDirAnnotation
import firrtl.stage.phases.DriverCompatibility.TopNameAnnotation
import logger.Logger

import scala.collection.mutable
import scala.util.DynamicVariable

object Driver {
  private val testContext = new DynamicVariable[Option[IOTestersContext]](None)
  private[iotesters] def ctx = testContext.value

  /** This executes a test harness that extends peek-poke tester upon a device under test
    * with an optionsManager to control all the options of the toolchain components
    *
    * @note passing an OptionsManager is not supported in the compatibility layer
    * @param dutGenerator    The device under test, a subclass of a Chisel3 module
    * @param testerGen       A peek poke tester with tests for the dut
    * @return                Returns true if all tests in testerGen pass
    */
  def execute[T <: Module](dutGenerator: () => T)(testerGen: T => PeekPokeTester[T]): Boolean =
    execute(Array(), dutGenerator)(testerGen)

  /** This executes the test with options provide from an array of string -- typically provided from the
    * command line
    *
    * @param args       A *main* style array of string options
    * @param dut        The device to be tested, (device-under-test)
    * @param testerGen  A peek-poke tester with test for the dey
    * @return           Returns true if all tests in testerGen pass
    */
  def execute[T <: Module](
    args:      Array[String],
    dut:       () => T,
    annos:     Seq[Annotation] = List()
  )(testerGen: T => PeekPokeTester[T]
  ): Boolean = {

    val inAnnos = annos ++: parseArgs(args)

    // compile design
    val (highFirrtl, module) = Compiler.elaborate(() => dut(), inAnnos)

    // attach a target directory to place the firrtl in
    val highFirrtlWithTargetDir = defaultTargetDir(highFirrtl, getTesterName(testerGen))
    val lowFirrtl = Compiler.toLowFirrtl(highFirrtlWithTargetDir)

    // create simulator context
    val simulator = Simulator.getSimulator(inAnnos, default = TreadleBackendAnnotation)
    val sim = simulator.createContext(lowFirrtl)
    val localCtx = IOTestersContext(sim)

    // run tests
    val result = testContext.withValue(Some(localCtx)) {
      Logger.makeScope(inAnnos) {
        testerGen(module).finish
      }
    }

    result
  }

  /** tries to replicate the target directory choosing from the original iotesters */
  private def defaultTargetDir(state: firrtl.CircuitState, testerName: => String): firrtl.CircuitState = {
    val annos = defaultTargetDir(state.annotations, testerName)
    state.copy(annotations = annos)
  }
  private def defaultTargetDir(annos: AnnotationSeq, testerName: => String): AnnotationSeq = {
    val (targetDirs, otherAnnos) = annos.partition(_.isInstanceOf[TargetDirAnnotation])
    val testDir = targetDirs.collectFirst { case TargetDirAnnotation(dir) => dir } match {
      case Some(".")   => "test_run_dir"
      case None        => "test_run_dir"
      case Some(other) => other
    }
    val path = s"${testDir}/${testerName}"
    TargetDirAnnotation(path) +: otherAnnos
  }

  /** derives the name of the PeekPoke tester class */
  private def getTesterName[M <: Module](testerGen: M => PeekPokeTester[M]): String = {
    val genClassName = testerGen.getClass.getName
    genClassName.split("""\$\$""").headOption.getOrElse("") + genClassName.hashCode.abs
  }

  /** This is just here as command line way to see what the options are
    * It will not successfully run
    * TODO: Look into dynamic class loading as way to make this main useful
    *
    * @param args unused args
    */
  def main(args: Array[String]): Unit = {
    execute(Array("--help"), null)(null)
  }

  /** Runs the ClassicTester and returns a Boolean indicating test success or failure
    * @@backendType determines whether the ClassicTester uses verilator or the firrtl interpreter to simulate
    * the circuit.
    * Will do intermediate compliation steps to setup the backend specified, including cpp compilation for the
    * verilator backend and firrtl IR compilation for the firrlt backend
    *
    * This apply method is a convenient short form of the [[Driver.execute()]] which has many more options
    *
    * The following tests a chisel CircuitX with a CircuitXTester setting the random number seed to a fixed value and
    * turning on verbose tester output.  The result of the overall test is put in testsPassed
    *
    * @example {{{
    *           val testsPassed = iotesters.Driver(() => new CircuitX, testerSeed = 0L, verbose = true) { circuitX =>
    *             CircuitXTester(circuitX)
    *           }
    * }}}
    *
    * @param dutGen      This is the device under test.
    * @param backendType The default backend is "firrtl" which uses the firrtl interpreter. Other options
    *                    "verilator" will use the verilator c++ simulation generator
    *                    "ivl" will use the Icarus Verilog simulation
    *                    "vcs" will use the VCS simulation
    *                    "vsim" will use the ModelSim/QuestaSim simulation
    * @param verbose     Setting this to true will make the tester display information on peeks,
    *                    pokes, steps, and expects.  By default only failed expects will be printed
    * @param testerSeed  Set the random number generator seed
    * @param testerGen   This is a test harness subclassing PeekPokeTester for dutGen,
    * @return            This will be true if all tests in the testerGen pass
    */
  def apply[T <: Module](
    dutGen:      () => T,
    backendType: String = "firrtl",
    verbose:     Boolean = false,
    testerSeed:  Long = System.currentTimeMillis(),
    annos:       Seq[Annotation] = List()
  )(testerGen:   T => PeekPokeTester[T]
  ): Boolean = {

    val args = List(
      "--backend-name",
      backendType,
      "--test-seed",
      testerSeed.toString
    ) ++ (if (verbose) List("--is-verbose") else List())
    execute(args.toArray, dutGen, annos = annos)(testerGen)
  }

  private def backendNameToAnnotation(name: String): Annotation = name match {
    case "treadle" => TreadleBackendAnnotation
    // we no longer support the firrtl interpreter
    case "firrtl"    => TreadleBackendAnnotation
    case "verilator" => VerilatorBackendAnnotation
    case other       => throw new NotImplementedError(s"Unsupported backend: $other")
  }

  private def parseArgs(args: Array[String]): AnnotationSeq = {
    var cmd = ""
    val annos = mutable.ListBuffer[Annotation]()
    args.foreach { a =>
      if (a.startsWith("--")) a match {
        case "--is-verbose" => println("TODO: support --is-verbose")
        case other          => cmd = other
      }
      else {
        require(cmd.nonEmpty, s"arguments should start with --! ${a}")
        cmd match {
          case "--backend-name" => annos.append(backendNameToAnnotation(a))
          case "--test-seed" =>
            val seed = a.toLong
            println(s"TODO: support --test-seed $seed")
          case "--target-dir" => annos.append(TargetDirAnnotation(a))
          case "--top-name" =>
            println(s"ignoring --top-name ${a}")
          case "--generate-vcd-output" if a == "on" =>
            annos.append(WriteVcdAnnotation)
          case other =>
            throw new NotImplementedError(s"Unsupported argument: $other $a")
        }
      }
    }
    annos.toList
  }

  /** Filter a sequence of annotations, ensuring problematic potential duplicates are removed.
    * @param annotations Seq[Annotation] to be filtered
    * @return filtered Seq[Annotation]
    */
  def filterAnnotations(annotations: Seq[Annotation]): Seq[Annotation] = {
    annotations.filterNot {
      case _: firrtl.options.TargetDirAnnotation   => true
      case _: logger.LogLevelAnnotation            => true
      case _: firrtl.stage.FirrtlCircuitAnnotation => true
      case _: firrtl.stage.InfoModeAnnotation      => true
      case _ => false
    }
  }
}

private case class IOTestersContext(
  backend:   SimulatorContext,
  isVerbose: Boolean = false,
  base:      Int = 16,
  seed:      Long = 0)
