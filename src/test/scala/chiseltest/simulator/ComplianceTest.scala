// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator

import chiseltest.experimental.sanitizeFileName
import firrtl.{AnnotationSeq, CircuitState}
import firrtl.options.TargetDirAnnotation
import org.scalatest.Outcome
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.DynamicVariable

/** Base class for all simulator compliance tests. */
abstract class ComplianceTest(sim: Simulator) extends AnyFlatSpec {
  behavior of sim.name

  // Provide test fixture data as part of 'global' context during test runs
  private var scalaTestContext = new DynamicVariable[Option[NoArgTest]](None)

  override def withFixture(test: NoArgTest): Outcome = {
    require(scalaTestContext.value.isEmpty)
    scalaTestContext.withValue(Some(test)) {
      super.withFixture(test)
    }
  }

  private def loadFirrtl(src: String, annos: AnnotationSeq = List()): CircuitState = {
    val state = CircuitState(firrtl.Parser.parse(src), annos)
    chiseltest.internal.Compiler.toLowFirrtl(state)
  }

  def load(src: String): SimulatorContext = {
    val targetDir = TargetDirAnnotation("test_run_dir/" + getTestName)
    sim.createContext(loadFirrtl(src, Seq(targetDir)))
  }

  def getTestName: String = {
    sanitizeFileName(scalaTestContext.value.get.name)
  }
}
