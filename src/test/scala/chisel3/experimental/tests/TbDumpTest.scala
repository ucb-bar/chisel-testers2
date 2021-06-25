package chisel3.experimental.tests

import org.scalatest._
import chisel3._
//import chisel3.tester._
import chisel3.tester.experimental.tbdump.tester._
import chisel3.tester.experimental.tbdump.util._
import chisel3.tester.experimental.TestOptionBuilder._
import chisel3.tester.internal.WriteVcdAnnotation
import chisel3.util._

class DualQueueModule[T <: Data](ioType: T, entries: Int) extends MultiIOModule {
  val in1 = IO(Flipped(Decoupled(ioType)))
  val in2 = IO(Flipped(Decoupled(ioType)))
  val out1 = IO(Decoupled(ioType))
  val out2 = IO(Decoupled(ioType))
  out1 <> Queue(in1, entries)
  out2 <> Queue(in2, entries)
}

class StaticModule[T <: Data](ioLit: T) extends MultiIOModule {
  val out = IO(Output(chiselTypeOf(ioLit)))
  out := ioLit
}

class StaticTest(module: StaticModule[UInt]) extends BaseDumpTester with TbDump {
  def dut = module

  dut.out.expect(42.U)
  finish
}

class ForkTest(module: DualQueueModule[UInt]) extends BaseDumpTester with TbDump {

  def dut = module

  dut.in1.initSource().setSourceClock(dut.clock)
  dut.out1.initSink().setSinkClock(dut.clock)
  dut.in2.initSource().setSourceClock(dut.clock)
  dut.out2.initSink().setSinkClock(dut.clock)

  fork {
       dut.in1.valid.poke(true.B)
       dut.out1.ready.poke(true.B)
       dut.in1.bits.poke(42.U)
       dut.out1.valid.waitAndStep(true.B, dut.clock)
       dut.out1.bits.expect(42.U)
  }
    .fork{
        dut.in2.valid.poke(true.B)
        dut.out2.ready.poke(true.B)
        dut.in2.bits.poke(43.U)
        dut.out2.valid.waitAndStep(true.B, dut.clock)
        dut.out2.bits.expect(43.U)
  }.joinAndStep(dut.clock)

  finish
}

import chisel3.tester._
class BasicTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Testers2"

  it should "test static circuits" in {
    test(new StaticModule(42.U)) { c =>
      new StaticTest(c)
    }
  }

  it should "test fork-join circuits" in {
    test(new DualQueueModule(chiselTypeOf(42.U), 4)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      new ForkTest(c)
    }
  }
}