// SPDX-License-Identifier: Apache-2.0

package chiseltest.backends.verilator

import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class BlackBoxAdderIO extends Bundle {
  val a = Input(UInt(8.W))
  val b = Input(UInt(8.W))
  val q = Output(UInt(8.W))
}

class BlackBoxAdder extends BlackBox with HasBlackBoxInline {
  val io = IO(new BlackBoxAdderIO).suggestName("io")
  setInline("BlackBoxAdder.v",
  """module BlackBoxAdder(a, b, q);
    |input [7:0] a, b;
    |output [7:0] q;
    |assign q = a + b;
    |endmodule
    |""".stripMargin)
}

class BlackBoxAdderWrapper extends Module {
  val io = IO(new BlackBoxAdderIO)
  val m = Module(new BlackBoxAdder)
  m.io <> io
}

class VerilogBlackBoxTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Verilator Backend"

  val annos = Seq(VerilatorBackendAnnotation)

  it should "support Verilog black boxes" in {
    val rand = new Random()
    val mask = (BigInt(1) << 8) - 1
    test(new BlackBoxAdderWrapper).withAnnotations(annos) { dut =>
      (0 until 1000).foreach { ii =>
        val a = BigInt(8, rand)
        val b = BigInt(8, rand)
        val q = (a + b) & mask
        dut.io.a.poke(a.U)
        dut.io.b.poke(b.U)
        dut.io.q.expect(q.U)
      }
    }
  }
}
