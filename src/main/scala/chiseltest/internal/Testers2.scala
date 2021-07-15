// SPDX-License-Identifier: Apache-2.0

package chiseltest.internal

import chisel3.Module
import chiseltest.TestResult
import chiseltest.simulator.WriteVcdAnnotation
import firrtl.AnnotationSeq
import firrtl.annotations.{Annotation, NoTargetAnnotation}
import firrtl.options.{HasShellOptions, ShellOption, Unserializable}
import firrtl.stage.phases.DriverCompatibility.TopNameAnnotation

import scala.util.DynamicVariable

// Internal common set of options to be understood by all backends
//TODO: Different backends will have different ways of expressing things like WriteVCD,
//TODO: there should be a formal mapping from testers2 options form to backend specific forms
@deprecated("Use annotation based options instead. See: .withAnnotations", "chisel-testers2 20190604")
case class TesterOptions(
  name:     String,
  writeVcd: Boolean) {
  def toAnnotations: AnnotationSeq = {
    Seq(
      Some(TopNameAnnotation(name)),
      if (writeVcd) { Some(WriteVcdAnnotation) }
      else None
    ).flatten
  }
}

trait TestOption extends Unserializable { this: Annotation => }
trait TestOptionObject extends NoTargetAnnotation with HasShellOptions with TestOption

case object LineCoverageAnnotation extends TestOptionObject {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "t-line-coverage",
      toAnnotationSeq = _ => Seq(LineCoverageAnnotation),
      helpText = "adds line coverage in VCS or Verilator"
    )
  )
}

case object ToggleCoverageAnnotation extends TestOptionObject {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "t-toggle-coverage",
      toAnnotationSeq = _ => Seq(ToggleCoverageAnnotation),
      helpText = "adds toggle coverage in VCS or Verilator"
    )
  )
}

case object BranchCoverageAnnotation extends TestOptionObject {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "t-branch-coverage",
      toAnnotationSeq = _ => Seq(ToggleCoverageAnnotation),
      helpText = "adds branch coverage in VCS"
    )
  )
}

case object ConditionalCoverageAnnotation extends TestOptionObject {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "t-conditional-coverage",
      toAnnotationSeq = _ => Seq(ToggleCoverageAnnotation),
      helpText = "adds conditional coverage in VCS"
    )
  )
}

case object StructuralCoverageAnnotation extends TestOptionObject {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "t-structural-coverage",
      toAnnotationSeq = _ => Seq(LineCoverageAnnotation),
      helpText = "adds all forms of structural in VCS or Verilator"
    )
  )
}

case object UserCoverageAnnotation extends TestOptionObject {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "t-user-coverage",
      toAnnotationSeq = _ => Seq(LineCoverageAnnotation),
      helpText = "adds user coverage in VCS or Verilator"
    )
  )
}

case object CachingAnnotation extends TestOptionObject {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "t-use-caching",
      toAnnotationSeq = _ => Seq(CachingAnnotation),
      helpText = "caches built simulations when using Verilator"
    )
  )
}

/** TODO: re-enable
  * case object VcsBackendAnnotation extends BackendAnnotation {
  *  val executive: BackendExecutive = VcsExecutive
  *
  *  val options: Seq[ShellOption[_]] = Seq(
  *    new ShellOption[Unit](
  *      longOption = "t-use-vcs",
  *      toAnnotationSeq = _ => Seq(VerilatorBackendAnnotation),
  *      helpText = "direct tester to use VCS backend"
  *    )
  *  )
  * }
  */

object Context {
  class Instance(val backend: BackendInterface, val env: TestEnvInterface) {}

  private var context = new DynamicVariable[Option[Instance]](None)

  def run[T <: Module](backend: BackendInstance[T], env: TestEnvInterface, testFn: T => Unit): TestResult = {
    require(context.value.isEmpty)
    val annotations = context.withValue(Some(new Instance(backend, env))) {
      backend.run(testFn)
    }
    new TestResult(annotations)
  }

  def apply(): Instance = context.value.get
}
