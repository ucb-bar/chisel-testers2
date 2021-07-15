// SPDX-License-Identifier: Apache-2.0

package chiseltest.iotesters.examples

import chisel3._
import chisel3.util._
import chiseltest.iotesters._
import chiseltest.simulator.{VerilatorUseJNI, WriteVcdAnnotation}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

object AluOpCode {
  val Add                   = 1.U
  val Sub                   = 2.U
  val AND                   = 3.U
  val OR                    = 4.U
  val XOR                   = 5.U
  val XNOR                  = 6.U
  val ShiftLeft             = 7.U
  val ShiftRightLogical     = 8.U
  val ShiftRightArithmetic  = 9.U
  val SetLessThan           = 10.U
  val SetLessThanUnsigned   = 11.U
  val PassA                 = 12.U
  val PassB                 = 13.U
  val length = 13
}

class ALU extends Module {
  val io = IO(new Bundle {
    val in1 = Input(UInt(32.W))
    val in2 = Input(UInt(32.W))
    val alu_opcode = Input(UInt(AluOpCode.length.W))
    val out = Output(UInt(32.W))
  })

  private val mux_alu_opcode = io.alu_opcode(AluOpCode.length - 1, 0)

  io.out := MuxLookup(mux_alu_opcode, "hDEADF00D".U,
    Array(
      AluOpCode.Add -> (io.in1 + io.in2),
      AluOpCode.Sub -> (io.in1 - io.in2),
      AluOpCode.AND -> (io.in1 & io.in2),
      AluOpCode.OR -> (io.in1 | io.in2),
      AluOpCode.XOR -> (io.in1 ^ io.in2),
      AluOpCode.XNOR -> (~(io.in1 ^ io.in2)),
      AluOpCode.ShiftLeft -> (io.in1 << io.in2(4, 0)),
      AluOpCode.ShiftRightLogical -> (io.in1 >> io.in2(4, 0)),

      //BUG ALERT does not convert back to UInt properly !????
      AluOpCode.ShiftRightArithmetic -> (io.in1.asSInt >> io.in2(4, 0)).asUInt, // Chisel only performs arithmetic right-shift on SInt
      AluOpCode.SetLessThan -> (io.in1.asSInt < io.in2.asSInt),
      AluOpCode.SetLessThanUnsigned -> (io.in1 < io.in2),
      AluOpCode.PassA -> io.in1,
      AluOpCode.PassB -> io.in2
    )
  )

  when(true.B) {
    printf("io.alu_opcode = %d mux_alu_opcode = %d io.in1 = %x io.in2 = %x io.out = %x\n",
      io.alu_opcode, mux_alu_opcode, io.in1, io.in2, io.out)
  }
}

class ALUBasicFunctionTester (c: ALU) extends PeekPokeTester (c) {

  // Add
  poke (c.io.in1, "h0001".U)
  poke (c.io.in2, "h0002".U)
  poke (c.io.alu_opcode, AluOpCode.Add)
  step (1)
  expect (c.io.out, "h0003".U)


  // Add should overflow to become 0
  poke (c.io.in1, "hFFFFFFFF".U)
  poke (c.io.in2, "h00000001".U)
  poke (c.io.alu_opcode, AluOpCode.Add)
  step (1)
  expect (c.io.out, "h00000000".U)

  // Basic OR
  poke (c.io.in1, "hAAAAAAAA".U)
  poke (c.io.in2, "h55555555".U)
  poke (c.io.alu_opcode, AluOpCode.OR)
  step (1)
  expect (c.io.out, "hFFFFFFFF".U)

  // Basic XOR
  poke (c.io.in1, "h00000007".U)
  poke (c.io.in2, "h00000002".U)
  poke (c.io.alu_opcode, AluOpCode.XOR)
  step (1)
  expect (c.io.out, "h00000005".U)

  // Basic XNOR
  poke (c.io.in1, "h00000007".U)
  poke (c.io.in2, "h00000002".U)
  poke (c.io.alu_opcode, AluOpCode.XNOR)
  step (1)
  expect (c.io.out, "hFFFFFFFA".U)

  // Left shift
  poke (c.io.in1, "h00000001".U)
  poke (c.io.in2, "h0000001F".U)
  poke (c.io.alu_opcode, AluOpCode.ShiftLeft)
  step (1)
  expect (c.io.out, "h80000000".U)

  // Logical right shift (fill top bits with 0's)
  poke (c.io.in1, "hF0000000".U)
  poke (c.io.in2, "h00000004".U)
  poke (c.io.alu_opcode, AluOpCode.ShiftRightLogical)
  step (1)
  expect (c.io.out, "h0F000000".U)

  // Logical right shift (fill top bits with 0's)
  poke (c.io.in1, "h00000001".U)
  poke (c.io.in2, "h00000001".U)
  poke (c.io.alu_opcode, AluOpCode.ShiftRightLogical)
  step (1)
  expect (c.io.out, "h00000000".U)

  // Arithmetic right shift (fill top bits with 1's)
  poke (c.io.in1, "hF0000000".U)
  poke (c.io.in2, "h00000004".U)
  poke (c.io.alu_opcode, AluOpCode.ShiftRightArithmetic)
  step (1)
  expect (c.io.out, "hFF000000".U)

  // Arithmetic right shift (should fill 0th bit with 1)
  poke (c.io.in1, "h80000001".U)
  poke (c.io.in2, "h00000001".U)
  poke (c.io.alu_opcode, AluOpCode.ShiftRightArithmetic)
  step (1)
  expect (c.io.out, "hC0000000".U)


  // SLT; compare 1 to -2147483647
  poke (c.io.in1, "h00000001".U)
  poke (c.io.in2, "h80000001".U)
  poke (c.io.alu_opcode, AluOpCode.SetLessThan)
  step (1)
  expect (c.io.out, "h00000000".U)

  // SLTU ; compare 1 to 2147483649
  poke (c.io.in1, "h00000001".U)
  poke (c.io.in2, "h80000001".U)
  poke (c.io.alu_opcode, AluOpCode.SetLessThanUnsigned)
  step (1)
  expect (c.io.out, "h00000001".U)

  // PASS A
  poke (c.io.in1, "h00000001".U)
  poke (c.io.in2, "h00000002".U)
  poke (c.io.alu_opcode, AluOpCode.PassA)
  step (1)
  expect (c.io.out, "h00000001".U)

  // PASS B
  poke (c.io.in1, "h00000001".U)
  poke (c.io.in2, "h00000002".U)
  poke (c.io.alu_opcode, AluOpCode.PassB)
  step (1)
  expect (c.io.out, "h00000002".U)
}

class ALUBizarreInputTester (c: ALU) extends PeekPokeTester (c) {

  // Incorrect opcode results in default out
  poke (c.io.in1, "h00000001".U)
  poke (c.io.in2, "h00000002".U)
  poke (c.io.alu_opcode, 15.U)
  step (1)
  expect (c.io.out, "hDEADF00D".U)

  // Inputs are both 32 bits; function should only operates on the lowest 32 bits
  poke (c.io.in1, "hFFFFFFFF".U)
  poke (c.io.in2, "h00000001".U)
  poke (c.io.alu_opcode, AluOpCode.Add)
  step (1)
  expect (c.io.out, "h00000000".U) // With 33 bit in/out, this would be h200000000

  // Input is 32 bits; subtract 1 should yield underflow
  poke (c.io.in1, "h00000000".U)
  poke (c.io.in2, "h00000001".U)
  poke (c.io.alu_opcode, AluOpCode.Sub)
  step (1)
  expect (c.io.out, "hFFFFFFFF".U)

  /*
  //THIS BREAKS. INPUT REFERENCE BIT WIDTH DOESN'T SEEM TO WORK.
  // Opcode is too long (5 bits instead of 4). 4-bit base opcode is Sub
  poke(c.io.in1, "h00000002".U)
  poke(c.io.in2, "h00000001".U)
  poke(c.io.alu_opcode, "h1F".U) // opcode = 18 should be sliced to opcode = 2
  step(1)
  expect(c.io.out, "h00000001".U) // With 5-bit opcode, would output
  "hDEADF00D".U
  step(1)
  */
}

class ALUTester extends AnyFlatSpec {
  behavior of "ALU"

  private val backendNames = Array[String] ("firrtl", "verilator")
  for (backendName <- backendNames) {
    it should s"compute data output according to alu_opcode (with  $backendName)" in {
      Driver(() => new ALU, backendName) {
        c => new ALUBasicFunctionTester(c)
      } should be(true)
    }

    it should s"not compute data with incorrect alu_opcode (with $backendName)" in {
      Driver(() => new ALU, backendName) {
        c => new ALUBizarreInputTester(c)
      } should be(true)
    }
  }
}

class ALUJNITest extends AnyFlatSpec {
  behavior of "ALU with JNI backend"

  it should s"compute data output according to alu_opcode (with Verilator  + JNI)" in {
    Driver(() => new ALU, "verilator", annos = Seq(WriteVcdAnnotation, VerilatorUseJNI)) {
      c => new ALUBasicFunctionTester(c)
    } should be(true)
  }
}