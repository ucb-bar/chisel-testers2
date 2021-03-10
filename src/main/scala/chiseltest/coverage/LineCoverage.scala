// SPDX-License-Identifier: Apache-2.0

package chiseltest.coverage

import chiseltest.internal.TestOptionObject
import firrtl._
import firrtl.annotations.{Annotation, CircuitTarget, ModuleTarget, NoTargetAnnotation, ReferenceTarget, SingleTargetAnnotation}
import firrtl.options.{Dependency, ShellOption}
import firrtl.stage.Forms
import firrtl.stage.TransformManager.TransformDependency
import firrtl.transforms.DedupModules

import scala.collection.mutable

case object LineCoverage extends TestOptionObject {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "t-line-coverage",
      toAnnotationSeq = _ => Seq(LineCoverage),
      helpText = "instruments the circuit and generates a line coverage report at the end of the test"
    )
  )
}

trait CoverageReportGeneratorAnnotation extends NoTargetAnnotation

case class LineCoverageReportGenerator() extends CoverageReportGeneratorAnnotation

case class LineCoverageAnnotation(target: ReferenceTarget, lines: Coverage.Lines) extends SingleTargetAnnotation[ReferenceTarget] {
  override def duplicate(n: ReferenceTarget) = copy(target = n)
}

object LineCoveragePass extends Transform with DependencyAPIMigration {
  val Prefix = "l"

  override def prerequisites: Seq[TransformDependency] = Forms.Checks
  // TODO: we might actually want to run after deduplication...
  override def optionalPrerequisiteOf: Seq[TransformDependency] = Seq(Dependency[DedupModules])
  override def invalidates(a: Transform): Boolean = false

  override protected def execute(state: CircuitState): CircuitState = {
    val newAnnos = mutable.ListBuffer[Annotation]()
    val c = CircuitTarget(state.circuit.main)
    val circuit = state.circuit.mapModule(onModule(_, c, newAnnos))
    val annos = newAnnos.toList ++ state.annotations
    CircuitState(circuit, annos)
  }

  private case class ModuleCtx(annos: mutable.ListBuffer[Annotation], namespace: Namespace, m: ModuleTarget, clk: ir.Expression)

  private def onModule(m: ir.DefModule, c: CircuitTarget, annos: mutable.ListBuffer[Annotation]): ir.DefModule = m match {
    case e: ir.ExtModule => e
    case mod: ir.Module =>
      val namespace = Namespace(mod)
      namespace.newName(Prefix)
      val ctx = ModuleCtx(annos, namespace, c.module(mod.name), Builder.findClock(mod))
      val bodyInfo = onStmt(mod.body, ctx)
      val body = addCover(bodyInfo, ctx)
      mod.copy(body = body)
  }

  private def onStmt(s: ir.Statement, ctx: ModuleCtx): (ir.Statement, Boolean, Seq[ir.Info]) = s match {
    case c @ ir.Conditionally(_, _, conseq, alt) =>
      val truInfo = onStmt(conseq, ctx)
      val falsInfo = onStmt(alt, ctx)
      val doCover = truInfo._2 || falsInfo._2
      val stmt = c.copy(conseq=addCover(truInfo, ctx), alt = addCover(falsInfo, ctx))
      (stmt, doCover, List(c.info))
    case ir.Block(stmts) =>
      val s = stmts.map(onStmt(_, ctx))
      val block = ir.Block(s.map(_._1))
      val doCover = s.map(_._2).foldLeft(false)(_ || _)
      val infos = s.flatMap(_._3)
      (block, doCover, infos)
    case ir.EmptyStmt => (ir.EmptyStmt, false, List())
    case v @ ir.Verification(ir.Formal.Cover, _, _, _, _, _) => (v, false, List(v.info))
    case other: ir.HasInfo => (other, true, List(other.info))
    case other => (other, false, List())
  }

  private def addCover(info: (ir.Statement, Boolean, Seq[ir.Info]), ctx: ModuleCtx): ir.Statement = {
    val (stmt, doCover, infos) = info
    if(!doCover) { stmt } else {
      val name = ctx.namespace.newName(Prefix)
      val lines = Coverage.infosToLines(infos)
      ctx.annos.prepend(LineCoverageAnnotation(ctx.m.ref(name), lines))
      val cover = ir.Verification(ir.Formal.Cover, ir.NoInfo, ctx.clk, Utils.True(), Utils.True(), ir.StringLit(""), name)
      ir.Block(cover, stmt)
    }
  }
}