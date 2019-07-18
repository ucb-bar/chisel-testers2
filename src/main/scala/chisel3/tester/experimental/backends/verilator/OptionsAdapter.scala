// See LICENSE for license details.

package chisel3.tester.experimental.backends.verilator

import firrtl.AnnotationSeq
import firrtl.annotations.{Annotation, NoTargetAnnotation}
import firrtl.options.{HasShellOptions, Phase, ShellOption, Unserializable}

/** This provides a mechanism to map between generic testers2 level annotations and
  * any differences the backend may have in expressing their intent.
  *
  */
class OptionsAdapter extends Phase {
  /** Currently the lack of a WriteVcd annotation adds a SuppressVerilatorVCD
    *
    * @param annos an annotation list
    * @return
    */
  override def transform(annos: AnnotationSeq): AnnotationSeq = {
    if(annos.contains(chisel3.tester.internal.WriteVcdAnnotation)) {
      annos
    } else {
      annos :+ SuppressVerilatorVcd
    }
  }
}

trait VerilatorOption extends NoTargetAnnotation with Unserializable {
  this: Annotation =>
}
trait VerilatorOptionObject extends VerilatorOption with HasShellOptions

/** Used to suppress verilator simulation vcd output.
  */
case object SuppressVerilatorVcd extends VerilatorOptionObject {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "tr-random-seed",
      toAnnotationSeq = _ => Seq(SuppressVerilatorVcd),
      helpText = "sets the seed for Treadle's random number generator"
    )
  )
}

/** A sequence string flags to add to verilator command line
  *
  * @param flags additional flags
  */
case class VerilatorFlags(flags: Seq[String]) extends VerilatorOption

/** CLI builder for VerilatorFlags
  */
case object VerilatorFlags extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[String](
      longOption = "t-verilator-flags",
      toAnnotationSeq = (flags: String) =>
        Seq(VerilatorFlags(flags.split(" +"))),
      helpText = "additional flags to pass to the verilator program"
    )
  )
}

/** A sequence string flags to add to verilator command line
  *
  * @param flags additional flags
  */
case class VerilatorCFlags(flags: Seq[String]) extends VerilatorOption

/** CLI builder for VerilatorCFlags
  */
case object VerilatorCFlags extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[String](
      longOption = "t-verilator-flags",
      toAnnotationSeq = (flags: String) =>
        Seq(VerilatorCFlags(flags.split(" +"))),
      helpText = "additional flags to pass to the c++ compiler"
    )
  )
}

/** A string specifying a file containing regex edits for verilator command line
  *
  * @param flags additional flags
  */
case class CommandEditsFile(flags: String) extends VerilatorOption

/** CLI builder for CommandEditsFile
  */
case object CommandEditsFile extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[String](
      longOption = "t-command-edits-file",
      toAnnotationSeq = (flags: String) => Seq(CommandEditsFile(flags)),
      helpText = "file of regex edits to apply to verilator program string"
    )
  )
}

/** A string specifying a file containing regex edits for verilator command line
  *
  * @param flags additional flags
  */
case class TestCommandOverride(flags: String) extends VerilatorOption

/** CLI builder for TestCommandOverride
  */
case object TestCommandOverride extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[String](
      longOption = "t-command-edits-file",
      toAnnotationSeq = (flags: String) => Seq(TestCommandOverride(flags)),
      helpText = "file of regex edits to apply to verilator program string"
    )
  )
}
