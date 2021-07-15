// See README.md for license details.

package chiseltest.iotesters

import chisel3._
import chisel3.util.Decoupled

class GcdInputBundle(val w: Int) extends Bundle {
  val value1 = UInt(w.W)
  val value2 = UInt(w.W)
}

class GcdOutputBundle(override val w: Int) extends GcdInputBundle(w) {
  val gcd    = UInt(w.W)
}

/**
  * Compute Gcd using subtraction method.
  * Subtracts the smaller from the larger until register y is zero.
  * value input register x is then the Gcd.
  * Unless first input is zero then the Gcd is y.
  * Can handle stalls on the producer or consumer side
  */
class DecoupledGcd(val bitWidth: Int) extends MultiIOModule {
  val input = IO(Flipped(Decoupled(new GcdInputBundle(bitWidth))))
  val output = IO(Decoupled(new GcdOutputBundle(bitWidth)))

  val xInitial    = Reg(UInt(bitWidth.W))
  val yInitial    = Reg(UInt(bitWidth.W))
  val x           = Reg(UInt(bitWidth.W))
  val y           = Reg(UInt(bitWidth.W))
  val busy        = RegInit(false.B)
  val resultValid = RegInit(false.B)

  input.ready := ! busy
  output.valid := resultValid
  output.bits := DontCare

  val cycle = RegInit(0.U(32.W))
  cycle := cycle + 1.U

//  printf("%d xi  %d yi  %d c  %d x  %d y  %d busy  %d valid  out\n",
//    xInitial, yInitial, cycle, x, y, busy.asUInt, resultValid.asUInt)

  when(busy)  {
    when(x >= y) {
      x := x - y
    }.otherwise {
      y := y - x
    }
    when(x === 0.U || y === 0.U) {
      when(x === 0.U) {
        output.bits.gcd := y
      }.otherwise {
        output.bits.gcd := x
      }

      output.bits.value1 := xInitial
      output.bits.value2 := yInitial
      resultValid := true.B

      when(output.ready && resultValid) {
        busy := false.B
        resultValid := false.B
      }
    }
  }.otherwise {
    when(input.valid) {
      val bundle = input.deq()
      x := bundle.value1
      y := bundle.value2
      xInitial := bundle.value1
      yInitial := bundle.value2
      busy := true.B
    }
  }
}
