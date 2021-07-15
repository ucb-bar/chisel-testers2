// SPDX-License-Identifier: Apache-2.0

package chiseltest.experimental.tests

import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import org.scalatest.freespec.AnyFreeSpec

class HasOddWidthSInt extends Module {
  val in = IO(Input(SInt(15.W)))
  val out = IO(Output(Bool()))

  val masked = ((in >> 2) << 2).asSInt

  out := masked === in
}

// The poke of a negative number into an SInt, FixedPoint, or Interval input that is not a standard word size
// would break in verilator if the poked value was not masked to the correct number of
// bits first. This was fixed by masking those values to the proper width before poking
class NegativeInputValuesTest extends AnyFreeSpec with ChiselScalatestTester {
  "Negative input values on odd width SInt should not cause verilator to fail" in {
    test(new HasOddWidthSInt).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      for(inputValue <- Seq(-4, -3, -2, -1, 0, 1, 2, 3, 4)) {
        dut.in.poke(inputValue.S)
        dut.clock.step()
        dut.out.expect((inputValue % 4 == 0).B)
      }
    }
  }
}
