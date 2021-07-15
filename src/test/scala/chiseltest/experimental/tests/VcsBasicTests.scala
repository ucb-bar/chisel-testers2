// SPDX-License-Identifier: Apache-2.0
package chiseltest.experimental.tests

import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class VcsBasicTests extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Testers2 with Vcs"

  val annos = Seq() // TODO: VcsBackendAnnotation)

  it should "build and simulate a basic test with input and output" ignore {
    assume(firrtl.FileUtils.isVCSAvailable)

    test(new Module {
      val io = IO(new Bundle {
        val in = Input(UInt(8.W))
        val out = Output(UInt(8.W))
      })
      io.out := RegNext(io.in, 0.U)
    }).withAnnotations(annos) { c =>
      c.io.in.poke(0.U)
      c.clock.step()
      c.io.out.expect(0.U)
      c.io.in.poke(42.U)
      c.clock.step()
      c.io.out.expect(42.U)
    }
  }
}
