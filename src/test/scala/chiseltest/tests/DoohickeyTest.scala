import chisel3._
import chisel3._
import chiseltest._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import org.scalatest._
import chiseltest.internal.VerilatorBackendAnnotation
import chiseltest.legacy.backends.verilator.VerilatorFlags
import chiseltest.legacy.backends.verilator.VerilatorCFlags
import chiseltest.internal.WriteVcdAnnotation
import chisel3._
import chisel3.experimental.IntParam

import chisel3.util._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import chisel3._
import chisel3.util._

class DualPortNewDataRAMBundle(Width: Int, addressBits: Int) extends Bundle {
  val clk = Input(Clock())
  val ram_wr_addr = Input(UInt(addressBits.W))
  val ram_we = Input(Bool())
  val ram_din = Input(UInt(Width.W))

  val ram_re = Input(Bool())
  val ram_dout = Output(UInt(Width.W))
  val ram_rd_addr = Input(UInt(addressBits.W))

  val ram_re2 = Input(Bool())
  val ram_dout2 = Output(UInt(Width.W))
  val ram_rd_addr2 = Input(UInt(addressBits.W))
  override def cloneType(): this.type = {
    new DualPortNewDataRAMBundle(Width, addressBits)
      .asInstanceOf[this.type]
  }
}

class DualPortNewDataRAM(Width: Int, addressBits: Int)
    extends BlackBox(
      Map(
        "WIDTH" -> IntParam(Width),
        "ADDR" -> IntParam(addressBits)
      )
    )
    with HasBlackBoxResource {
  val io = IO(
    new DualPortNewDataRAMBundle(Width, addressBits)
  )

  addResource("/DualPortNewDataRAM.v")
}


class DoohickeyBundle() extends Bundle {
  val readAddress = Input(Vec(1, UInt(4.W)))
  val readEnable = Input(Vec(1, Bool()))
  val readData = Output(Vec(1, UInt(64.W)))
  val writeAddress = Input(UInt(4.W))
  val writeEnable = Input(Bool())
  val writeData = Input(UInt(64.W))

  override def cloneType(): this.type = {
    new DoohickeyBundle().asInstanceOf[this.type]
  }
}

class Doohickey() extends Module {
  val io = IO(new DoohickeyBundle)

  val memory = {
      Module(
        new DualPortNewDataRAM(Width = io.readData.head.getWidth, addressBits = io.readAddress.head.getWidth)
      ).io
  }

  memory.clk := clock
  memory.ram_wr_addr := io.writeAddress
  memory.ram_we := io.writeEnable
  memory.ram_din := io.writeData.asUInt
  memory.ram_rd_addr := io.readAddress.head
  memory.ram_re := io.readEnable.head
  if (io.readEnable.length > 1) {
    memory.ram_rd_addr2 := io.readAddress(1)
    memory.ram_re2 := io.readEnable(1)
  } else {
    memory.ram_rd_addr2 := DontCare
    memory.ram_re2 := false.B
  }

  io.readData.head := memory.ram_dout.asTypeOf(io.readData.head)
  if (io.readEnable.length > 1) {
    io.readData(1) := memory.ram_dout2.asTypeOf(io.readData.head)
  }
}

class DoohickeyTests extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Doohickey"
  val annos = Seq(
    VerilatorBackendAnnotation,
    WriteVcdAnnotation
  )

  val dataWidth = 32

  it should "do it" in {
    test(new Doohickey).withAnnotations(annos) { dut =>
  dut.reset.poke(true.B)
  dut.clock.step(3)
  dut.reset.poke(false.B)
    }
  }
}
