// SPDX-License-Identifier: Apache-2.0

package chiseltest.tests

import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import firrtl.AnnotationSeq
import org.scalatest._
import org.scalatest.freespec.AnyFreeSpec

/**
  * circuit that illustrates usage of async register
  * @param resetValue value on reset
  */
class AsyncResetRegModule(resetValue: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(8.W))
    val out = Output(UInt(8.W))
  })
  withClockAndReset(clock, reset.asAsyncReset) {
    val reg = RegInit(resetValue.U(8.W))
    reg := io.in
    io.out := reg
  }

}

class AsyncResetFeedbackModule() extends Module {
  val io = IO(new Bundle {
    val out = Output(Vec(2, UInt(1.W)))
  })
  withClockAndReset(clock, reset.asAsyncReset) {
    val reg0 = RegInit(0.U(8.W))
    val reg1 = RegInit(1.U(8.W))
    reg0 := reg1
    reg1 := reg0

    io.out(0) := reg0
    io.out(1) := reg1
  }
}

class AsyncResetTest extends AnyFreeSpec with ChiselScalatestTester {

  val annotations: AnnotationSeq = {
    if(true) Seq() else Seq(VerilatorBackendAnnotation)
  }

  "register is set to reset value (1), after tester startup's default reset" in {
    test(new AsyncResetRegModule(1)).withAnnotations(annotations) { dut =>
      dut.io.out.expect(1.U)
    }
  }

  "register is set to reset value (7), despite poke of input" in {
    test(new AsyncResetRegModule(7)).withAnnotations(annotations) { dut =>
      dut.io.in.poke(3.U)
      dut.io.out.expect(7.U)
    }
  }

  "register is set to input, because reset is off and clock has stepped" in {
    test(new AsyncResetRegModule(7)).withAnnotations(annotations) { dut =>
      dut.io.in.poke(3.U)
      dut.io.out.expect(7.U)
      dut.clock.step()
      dut.io.out.expect(3.U)
    }
  }

  "asserting reset holds reset value of register even as clock changes" in {
    test(new AsyncResetRegModule(11)).withAnnotations(annotations) { dut =>
      dut.io.in.poke(7.U)
      dut.io.out.expect(11.U)

      dut.clock.step()
      dut.io.out.expect(7.U)

      dut.reset.poke(true.B)
      dut.io.in.poke(11.U)

      dut.clock.step()
      dut.io.out.expect(11.U)

      dut.io.in.poke(8.U)
      dut.io.out.expect(11.U)
      dut.clock.step()
      dut.io.out.expect(11.U)
    }
  }

  "reset value maintained while reset asserted, goes to input value when de-asserted" in {
    test(new AsyncResetRegModule(11)).withAnnotations(annotations) { dut =>
      dut.reset.poke(true.B)
      dut.io.out.expect(11.U)

      dut.io.in.poke(7.U)

      dut.io.out.expect(11.U)
      dut.clock.step()
      dut.io.out.expect(11.U)

      dut.reset.poke(false.B)
      dut.io.in.expect(7.U)
      dut.clock.step()
      dut.io.in.expect(7.U)
    }
  }

  "mutually toggling registers, revert to reset values when reset asserted, resuming toggling when de-asserted" in {
    test(new AsyncResetFeedbackModule).withAnnotations(annotations) { dut =>
      dut.io.out(0).expect(0.U)
      dut.io.out(1).expect(1.U)

      dut.clock.step()
      dut.io.out(0).expect(1.U)
      dut.io.out(1).expect(0.U)

      dut.clock.step()
      dut.io.out(0).expect(0.U)
      dut.io.out(1).expect(1.U)

      dut.reset.poke(true.B)
      dut.io.out(0).expect(0.U)
      dut.io.out(1).expect(1.U)

      dut.clock.step()
      dut.io.out(0).expect(0.U)
      dut.io.out(1).expect(1.U)

      dut.reset.poke(false.B)
      dut.io.out(0).expect(0.U)
      dut.io.out(1).expect(1.U)

      dut.clock.step()
      dut.io.out(0).expect(1.U)
      dut.io.out(1).expect(0.U)

      dut.clock.step()
      dut.io.out(0).expect(0.U)
      dut.io.out(1).expect(1.U)

      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)

      dut.io.out(0).expect(0.U)
      dut.io.out(1).expect(1.U)

    }
  }
}

