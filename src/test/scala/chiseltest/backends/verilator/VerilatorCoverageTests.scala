// SPDX-License-Identifier: Apache-2.0

package chiseltest.backends.verilator

import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.{ByteArrayOutputStream, File, PrintStream}

class VerilatorCoverageTests extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Testers2"

  // TODO: this test needs to use a non-empty module in order to actually check anything!
  it should "allow specifying toggle coverage for Verilator" ignore {
    val coverageName = "test_run_dir/Testers2_should_allow_specifying_toggle_coverage_for_Verilator/logs/coverage.dat"
    val coverage = new File(coverageName)
    val outputStream = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(outputStream)) {
      test(new Module {}).withAnnotations(Seq(VerilatorBackendAnnotation, ToggleCoverageAnnotation)) { c => }
    }
    val output = outputStream.toString
    assert(coverage.exists())
    output should include("--coverage-toggle")
    output should not include("--coverage-line")
  }

  // TODO: this test needs to use a non-empty module in order to actually check anything!
  it should "allow specifying line coverage for Verilator" ignore {
    val coverageName = "test_run_dir/Testers2_should_allow_specifying_line_coverage_for_Verilator/logs/coverage.dat"
    val coverage = new File(coverageName)
    val outputStream = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(outputStream)) {
      test(new Module {}).withAnnotations(Seq(VerilatorBackendAnnotation, LineCoverageAnnotation)) { c => }
    }
    val output = outputStream.toString
    assert(coverage.exists())
    output should not include("--coverage-toggle")
    output should include("--coverage-line")
  }

  // TODO: this test needs to use a non-empty module in order to actually check anything!
  it should "allow specifying structural coverage for Verilator" ignore {
    val coverageName = "test_run_dir/Testers2_should_allow_specifying_structural_coverage_for_Verilator/logs/coverage.dat"
    val coverage = new File(coverageName)
    val outputStream = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(outputStream)) {
      test(new Module {}).withAnnotations(Seq(VerilatorBackendAnnotation, StructuralCoverageAnnotation)) { c => }
    }
    val output = outputStream.toString
    assert(coverage.exists())
    output should include("--coverage-toggle")
    output should include("--coverage-line")
  }

  // TODO: this test needs to use a non-empty module in order to actually check anything!
  it should "specify user coverage for Verilator by default" ignore {
    val coverageName = "test_run_dir/Testers2_should_allow_specifying_user_coverage_for_Verilator/logs/coverage.dat"
    val coverage = new File(coverageName)
    val outputStream = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(outputStream)) {
      test(new Module {}).withAnnotations(Seq(VerilatorBackendAnnotation)) { c => }
    }
    val output = outputStream.toString
    output should not include("--coverage-toggle")
    output should not include("--coverage-line")
    output should include("--coverage-user")
  }

  // TODO: this test needs to use a non-empty module in order to actually check anything!
  it should "allow stacking coverage for Verilator" ignore {
    val coverageName = "test_run_dir/Testers2_should_allow_stacking_coverage_for_Verilator/logs/coverage.dat"
    val coverage = new File(coverageName)
    val outputStream = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(outputStream)) {
      test(new Module {}).withAnnotations(Seq(VerilatorBackendAnnotation, UserCoverageAnnotation, StructuralCoverageAnnotation)) { c => }
    }
    val output = outputStream.toString
    assert(coverage.exists())
    output should include("--coverage-toggle")
    output should include("--coverage-line")
    output should include("--coverage-user")
  }
}
