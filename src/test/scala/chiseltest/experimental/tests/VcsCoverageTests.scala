// SPDX-License-Identifier: Apache-2.0

package chiseltest.experimental.tests

import java.io.{ByteArrayOutputStream, File, PrintStream}

import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
/*
class VcsCoverageTests extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Testers2"

  it should "allow specifying line coverage for Vcs" in {
    assume(firrtl.FileUtils.isVCSAvailable)

    val outputStream = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(outputStream)) {
      test(new Module {}).withAnnotations(Seq(VcsBackendAnnotation, LineCoverageAnnotation)) { c => }
    }
    val output = outputStream.toString
    output.contains("-cm line") should be(true)
  }

  it should "allow specifying toggle coverage for Vcs" in {
    assume(firrtl.FileUtils.isVCSAvailable)

    val outputStream = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(outputStream)) {
      test(new Module {}).withAnnotations(Seq(VcsBackendAnnotation, ToggleCoverageAnnotation)) { c => }
    }
    val output = outputStream.toString
    output.contains("-cm tgl") should be(true)
  }

  it should "allow specifying branch coverage for Vcs" in {
    assume(firrtl.FileUtils.isVCSAvailable)

    val outputStream = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(outputStream)) {
      test(new Module {}).withAnnotations(Seq(VcsBackendAnnotation, BranchCoverageAnnotation)) { c => }
    }
    val output = outputStream.toString
    output.contains("-cm branch") should be(true)
  }

  it should "allow specifying conditional coverage for Vcs" in {
    assume(firrtl.FileUtils.isVCSAvailable)

    val outputStream = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(outputStream)) {
      test(new Module() {}).withAnnotations(Seq(VcsBackendAnnotation, ConditionalCoverageAnnotation)) { c => }
    }
    val output = outputStream.toString
    output.contains("-cm cond") should be(true)
  }

  it should "allow specifying structural coverage for Vcs" in {
    assume(firrtl.FileUtils.isVCSAvailable)

    val outputStream = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(outputStream)) {
      test(new Module {}).withAnnotations(Seq(VcsBackendAnnotation, StructuralCoverageAnnotation)) { c => }
    }
    val output = outputStream.toString
    output.contains("-cm line+tgl+branch+cond") should be(true)
  }

  it should "allow specifying user coverage for Vcs" in {
    assume(firrtl.FileUtils.isVCSAvailable)

    val outputStream = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(outputStream)) {
      test(new Module {}).withAnnotations(Seq(VcsBackendAnnotation, UserCoverageAnnotation)) { c => }
    }
    val output = outputStream.toString
    output.contains("-cm assert") should be(true)
  }

  it should "allow stacking coverage for Vcs" in {
    assume(firrtl.FileUtils.isVCSAvailable)

    val outputStream = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(outputStream)) {
      test(new Module {}).withAnnotations(Seq(VcsBackendAnnotation, UserCoverageAnnotation, StructuralCoverageAnnotation)) { c => }
    }
    val output = outputStream.toString
    output.contains("-cm line+tgl+branch+cond+assert") should be(true)
  }
}
*/