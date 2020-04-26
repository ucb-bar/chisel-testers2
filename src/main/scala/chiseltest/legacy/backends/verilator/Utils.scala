// See LICENSE for license details.

package chiseltest.legacy.backends.verilator

import java.io.File

import chisel3._
import chisel3.experimental._
import chisel3.internal.InstanceId
import chisel3.internal.firrtl.{Circuit, Port}
import treadle.utils.BitMasks

import scala.collection.mutable.ArrayBuffer
import scala.sys.process._

/** @todo remove this. */
private[chiseltest] object getTopPortsDataName {
  def apply(name: String, data: Data): Seq[(Element, String)] = data match {
    case e: Element => Seq(e -> name)
    case b: Record => b.elements.toSeq flatMap {case (n, e) => apply(s"${name}_$n", e)}
    case v: Vec[_] => v.zipWithIndex flatMap {case (e, i) => apply(s"${name}_$i", e)}
  }

  def apply(dut: MultiIOModule, circuit: firrtl.ir.Circuit, separator: String = "."): Seq[(Element, String)] = {
    val topPorts = circuit.modules.flatMap {
      case module@firrtl.ir.Module(_, name, _, _) if name == circuit.main => module.ports.map(_.name)
      case _ => Nil
    }
    dut.getPorts.flatMap { case chisel3.internal.firrtl.Port(data, _) =>
      apply(data.pathName, data).filter { case (_, n) =>
        topPorts.contains(n.split("\\.", 2).last)
      }.map { case (e, n) => (e, n replace(".", separator)) }
    }
  }
}

/** An EditableBuildCSimulatorCommand provides methods for assembling a system command string from provided flags and editing specifications.
  * This is a trait to facilitate expansion (for more C-based simulators) and testing.
  */
trait EditableBuildCSimulatorCommand {
  val prefix: String  // prefix to be used for error messages

  /** If we have a list of black box verilog implementations, return a sequence suitable for sourcing the file containing the list.
    *
    * @param dir - directory in which the file should exist
    * @return sequence of strings (suitable for passing as arguments to the simulator builder) specifying a flag and the absolute path to the file.
    */
  def blackBoxVerilogList(dir: java.io.File): Seq[String] = {
    val list_file = new File(dir, firrtl.transforms.BlackBoxSourceHelper.fileListName)
    if(list_file.exists()) {
      Seq("-f", list_file.getAbsolutePath)
    } else {
      Seq.empty[String]
    }
  }

  /** Compose user-supplied flags with the default flags.
    * @param topModule - the name of the module to be simulated
    * @param dir - the directory in which to build the simulation
    * @param moreIvlFlags - general flags for the build process
    * @param moreIvlCFlags - C flags for the build process
    * @return tuple containing a sequence of the composed general flags and a sequence of the composed C flags
    */
  def composeFlags(
                      topModule: String,
                      dir: java.io.File,
                      moreIvlFlags: Seq[String] = Seq.empty[String],
                      moreIvlCFlags: Seq[String] = Seq.empty[String]): (Seq[String], Seq[String])

  /** Given two sets of flags (non-CFlags and CFlags), return the composed command (prior to editting).
    * @param topModule - the name of the module to be simulated
    * @param dir - the directory in which to build the simulation
    * @param flags - general flags for the build process
    * @param cFlags - C flags for the build process
    * @return a string (suitable for "bash -c") to build the simulator.
    */
  def composeCommand(
                        topModule: String,
                        dir: java.io.File,
                        flags: Seq[String],
                        cFlags: Seq[String]
                    ): String

  /** Edit a C simulator build string.
    *
    * @param buildCommand - generated command line to be passed to the build process ("bash -c <cmd>")
    * @param editCommands - commands to edit the generated command line
    * @return edited command string
    */
  def editCSimulatorCommand(
                               buildCommand: String,
                               editCommands: String
                           ): String = {

    val commandEditor = CommandEditor(editCommands, prefix)
    val editedCommand = commandEditor(buildCommand)
    editedCommand
  }

  /** Construct a command to build a C-based simulator.
    *
    * @param topModule - the name of the module to be simulated
    * @param dir - the directory in which to build the simulation
    * @param flags - user flags to be passed to the build process - these will be composed with the default flags for the builder
    * @param cFlags - user C flags to be passed to the build process - these will be composed with the default C flags for the builder
    * @return string representing the bash command to be executed
    *
    * @note This method will call `composeFlags()` internally, so the flag parameters should '''NOT''' include the default flags for the builder.
    */
  def constructCSimulatorCommand(
                                         topModule: String,
                                         dir: java.io.File,
                                         harness:  java.io.File,
                                         flags: Seq[String] = Seq.empty[String],
                                         cFlags: Seq[String] = Seq.empty[String]
                                     ): String
}

private[chiseltest] object verilogToIVL extends EditableBuildCSimulatorCommand {
  val prefix = "ivl-command-edit"
  def composeCommand(
                      topModule: String,
                      dir: java.io.File,
                      flags: Seq[String],
                      cFlags: Seq[String]
                    ): String = {
    Seq("cd", dir.toString, "&&") ++
      Seq("g++") ++ cFlags ++ Seq("vpi.cpp", "vpi_register.cpp", "&&") ++
      Seq("iverilog") ++ flags mkString " "
  }

  def composeFlags(
               topModule: String,
               dir: java.io.File,
               moreIvlFlags: Seq[String] = Seq.empty[String],
               moreIvlCFlags: Seq[String] = Seq.empty[String]): (Seq[String], Seq[String]) = {

    val ivlFlags = Seq(
      "-m ./%s/%s.vpi".format(dir.toString, topModule),
      "-g2005-sv",
      "-DCLOCK_PERIOD=1"
    ) ++ moreIvlFlags

    val ivlCFlags = Seq(
      s"-o $topModule.vpi",
      "-D__ICARUS__",
      "-I$IVL_HOME",
      s"-I$dir",
      "-fPIC",
      "-std=c++11",
      "-lvpi",
      "-lveriuser",
      "-shared"
    ) ++ moreIvlCFlags

    (ivlFlags, ivlCFlags)
  }

  def constructCSimulatorCommand(
                                    topModule: String,
                                    dir: java.io.File,
                                    harness:  java.io.File,
                                    iFlags: Seq[String] = Seq.empty[String],
                                    iCFlags: Seq[String] = Seq.empty[String]
                                ): String = {

    val (cFlags, cCFlags) = composeFlags(topModule, dir,
      iFlags ++ blackBoxVerilogList(dir) ++ Seq("-o", topModule, s"$topModule.v", harness.toString),
      iCFlags
    )

    composeCommand(topModule, dir, cFlags, cCFlags)
  }

  def apply(
    topModule: String,
    dir: java.io.File,
    ivlHarness: java.io.File,
    moreIvlFlags: Seq[String] = Seq.empty[String],
    moreIvlCFlags: Seq[String] = Seq.empty[String],
    editCommands: String = ""): ProcessBuilder = {

    val finalCommand = editCSimulatorCommand(constructCSimulatorCommand(topModule, dir, ivlHarness, moreIvlFlags, moreIvlCFlags), editCommands)
    println(s"$finalCommand")

    Seq("bash", "-c", finalCommand)
  }
}

private[chiseltest] object verilogToVCS extends EditableBuildCSimulatorCommand {
  val prefix = "vcs-command-edit"
  override def composeCommand(
                                 topModule: String,
                                 dir: java.io.File,
                                 flags: Seq[String],
                                 cFlags: Seq[String]): String = {
    Seq("cd", dir.toString, "&&", "vcs") ++ flags mkString " "

  }


  def composeFlags(
                      topModule: String,
                      dir: java.io.File,
                      moreVcsFlags: Seq[String] = Seq.empty[String],
                      moreVcsCFlags: Seq[String] = Seq.empty[String]): (Seq[String], Seq[String]) = {

    val ccFlags = Seq("-I$VCS_HOME/include", "-I$dir", "-fPIC", "-std=c++11") ++ moreVcsCFlags

    val vcsFlags = Seq("-full64",
      "-quiet",
      "-timescale=1ns/1ps",
      "-debug_pp",
      s"-Mdir=$topModule.csrc",
      "+v2k", "+vpi",
      "+vcs+lic+wait",
      "+vcs+initreg+random",
      "+define+CLOCK_PERIOD=1",
      "-P", "vpi.tab",
      "-cpp", "g++", "-O2", "-LDFLAGS", "-lstdc++",
      "-CFLAGS", "\"%s\"".format(ccFlags mkString " ")) ++
      moreVcsFlags

    (vcsFlags, ccFlags)
  }

  def constructCSimulatorCommand(
                                    topModule: String,
                                    dir: java.io.File,
                                    harness:  java.io.File,
                                    iFlags: Seq[String] = Seq.empty[String],
                                    iCFlags: Seq[String] = Seq.empty[String]
                                ): String = {

    val (cFlags, cCFlags) = composeFlags(topModule, dir,
      iFlags ++ blackBoxVerilogList(dir) ++ Seq("-o", topModule, s"$topModule.v", harness.toString, "vpi.cpp"),
      iCFlags
    )

    composeCommand(topModule, dir, cFlags, cCFlags)
  }

  def apply(
    topModule: String,
    dir: java.io.File,
    vcsHarness: java.io.File,
    moreVcsFlags: Seq[String] = Seq.empty[String],
    moreVcsCFlags: Seq[String] = Seq.empty[String],
    editCommands: String = ""): ProcessBuilder = {

    val finalCommand = editCSimulatorCommand(constructCSimulatorCommand(topModule, dir, vcsHarness, moreVcsFlags, moreVcsCFlags), editCommands)
    println(s"$finalCommand")

    Seq("bash", "-c", finalCommand)
  }
}

private[chiseltest] object verilogToVerilator extends EditableBuildCSimulatorCommand {
  val prefix = "verilator-command-edit"
  override def composeCommand(
                                 topModule: String,
                                 dir: java.io.File,
                                 flags: Seq[String],
                                 cFlags: Seq[String]): String = {
    Seq("cd", dir.getAbsolutePath, "&&", "verilator", "--cc", s"$topModule.v") ++ flags mkString " "

  }

  def composeFlags(
               topModule: String,
               dir: File,
               moreVerilatorFlags: Seq[String] = Seq.empty[String],
               moreVerilatorCFlags: Seq[String] = Seq.empty[String]): (Seq[String], Seq[String]) = {

    val ccFlags = Seq(
      "-Wno-undefined-bool-conversion",
      "-O1",
      s"-DTOP_TYPE=V$topModule",
      "-DVL_USER_FINISH",
      s"-include V$topModule.h"
    ) ++ moreVerilatorCFlags

    val verilatorFlags = Seq("--assert",
      "-Wno-fatal",
      "-Wno-WIDTH",
      "-Wno-STMTDLY",
      "-O1",
      "--top-module", topModule,
      "+define+TOP_TYPE=V" + topModule,
      s"+define+PRINTF_COND=!$topModule.reset",
      s"+define+STOP_COND=!$topModule.reset",
      "-CFLAGS", "\"%s\"".format(ccFlags mkString " "),
      "-Mdir", dir.getAbsolutePath
    ) ++ moreVerilatorFlags
    (verilatorFlags, ccFlags)
  }

  def constructCSimulatorCommand(
                                    topModule: String,
                                    dir: java.io.File,
                                    harness:  java.io.File,
                                    iFlags: Seq[String] = Seq.empty[String],
                                    iCFlags: Seq[String] = Seq.empty[String]
                                ): String = {

    val (cFlags, cCFlags) = composeFlags(topModule, dir,
      blackBoxVerilogList(dir) ++ Seq("--exe", harness.getAbsolutePath) ++ iFlags,
      iCFlags
    )

    composeCommand(topModule, dir, cFlags, cCFlags)
  }

  def apply(
               topModule: String,
               dir: File,
               verilatorHarness: File,
               moreVerilatorFlags: Seq[String] = Seq.empty[String],
               moreVerilatorCFlags: Seq[String] = Seq.empty[String],
               editCommands: String = ""): ProcessBuilder = {

    val finalCommand = editCSimulatorCommand(constructCSimulatorCommand(topModule, dir, verilatorHarness, moreVerilatorFlags, moreVerilatorCFlags), editCommands)
    println(s"$finalCommand")

    Seq("bash", "-c", finalCommand)
  }
}

private[chiseltest] case class BackendException(b: String)
  extends Exception(s"Unknown backend: $b. Backend should be firrtl, verilator, ivl, vcs, or glsim")

private[chiseltest] case class TestApplicationException(exitVal: Int, lastMessage: String)
  extends RuntimeException(lastMessage)

private[chiseltest] object TesterProcess {
  def apply(cmd: Seq[String], logs: ArrayBuffer[String]): Process = {
    require(new java.io.File(cmd.head).exists, s"${cmd.head} doesn't exist")
    val processBuilder = Process(cmd mkString " ")
    // This makes everything written to stderr get added as lines to logs
    val processLogger = ProcessLogger(println, logs += _) // don't log stdout
    processBuilder run processLogger
  }
  //TODO: must figure this out before PR
  def kill(sim: SimApiInterface) {
    while(!sim.exitValue.isCompleted) sim.process.destroy
    println("Exit Code: %d".format(sim.process.exitValue))
  }
//  def kill(p: IVLBackend) {
//    kill(p.simApiInterface)
//  }
//  def kill(p: VCSBackend) {
//    kill(p.simApiInterface)
//  }
  def kill(p: VerilatorBackend[_]) {
    kill(p.simApiInterface)
  }
//  def kill(p: FirrtlTerpBackend) {
//  }
}

object Utils {

  /** Converts an unsigned BigInt of width X to it's signed value
    * Basically if msb is set convert it to a negative number
    *
    * @param unsigned an unsigned value that should be converted to signed
    * @param width    the width of the target (this helps to determine if sign (MSB) is set
    * @return         a signed BigInt
    */
  def unsignedBigIntToSigned(unsigned: BigInt, width: Int): BigInt = {
    val bitMasks = BitMasks.getBitMasksBigs(width)

    if (unsigned < 0) {
      unsigned
    } else {
      if (bitMasks.isMsbSet(unsigned)) {
        (unsigned & bitMasks.allBitsMask) - bitMasks.nextPowerOfTwo
      } else {
        unsigned & bitMasks.allBitsMask
      }
    }
  }
}

