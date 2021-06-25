package chiseltest.tests

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util.{Counter, Decoupled}
import chiseltest.experimental.TestOptionBuilder.ChiselScalatestOptionBuilder
import chiseltest.internal.WriteVcdAnnotation
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.util.Random

class GcdInputBundle(val w: Int) extends Bundle {
  val value1 = UInt(w.W)
  val value2 = UInt(w.W)
}

class GcdOutputBundle(val w: Int) extends Bundle {
  val value1 = UInt(w.W)
  val value2 = UInt(w.W)
  val gcd    = UInt(w.W)
}
/**
  * Compute GCD using subtraction method.
  * Subtracts the smaller of registers x and y from the larger until register y is zero.
  * value input register x is then the Gcd
  * returns a packet of information with the two input values and their GCD
  */
class DecoupledGcd(width: Int) extends Module {

  val input = IO(Flipped(Decoupled(new GcdInputBundle(width))))
  val output = IO(Decoupled(new GcdOutputBundle(width)))

  val xInitial    = Reg(UInt())
  val yInitial    = Reg(UInt())
  val x           = Reg(UInt())
  val y           = Reg(UInt())
  val busy        = RegInit(false.B)

  input.ready := ! busy
  output.bits := DontCare
  output.valid := false.B

  when(busy)  {
    // during computation keep subtracting the smaller from the larger
    when(y > 0.U) {
      when(x > y) {
        x := x - y
      }.otherwise {
        y := y - x
      }
    }.otherwise {
      // when y becomes zero computation is over, signal valid data to output
      output.bits.value1 := xInitial
      output.bits.value2 := yInitial
      output.bits.gcd := x
      output.valid := true.B
      busy := ! output.ready
    }
  }.otherwise {
    when(input.valid) {
      // valid data available and no computation in progress, grab new values and start
      val bundle = input.deq()
      x := bundle.value1
      y := bundle.value2
      xInitial := bundle.value1
      yInitial := bundle.value2
      busy := true.B
      output.valid := false.B
    }
  }

  val cycles = IO(Output(UInt(32.W)))

  val cyclesCounter = RegInit(0.U(32.W))
  cyclesCounter := cyclesCounter +% 1.U
  cycles := cyclesCounter
}

class DecoupledBackPressureInputStallTest extends AnyFreeSpec with ChiselScalatestTester {
  "This test randomly exerts backpressure and input delays" in {

    // runs a thunk with specified delay unless the seed and the delay are greater than zero
    def makeDelay(seed: Int)(thunk: Int => Unit): Unit = {
      if (seed > 0) {
        val delay = Random.nextInt(seed)
        if (delay > 0) {
          thunk(delay)
        }
      }
    }

    def runTest(dut: DecoupledGcd, inputDelayRand: Int = 0, outputDelayRand: Int = 0): Int = {
      // reset the dut before we use it
      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)

      dut.input.initSource().setSourceClock(dut.clock)
      dut.output.initSink().setSinkClock(dut.clock)

      val testValues = for {x <- 1 to 10; y <- 1 to 10} yield (x, y)
      val inputSeq = testValues.map { case (x, y) =>
        new GcdInputBundle(16).Lit(_.value1 -> x.U, _.value2 -> y.U)
      }
      val resultSeq = testValues.map { case (x, y) =>
        new GcdOutputBundle(16).Lit(_.value1 -> x.U, _.value2 -> y.U, _.gcd -> BigInt(x).gcd(BigInt(y)).U)
      }

      fork {
        for (stimulus <- inputSeq) {
          dut.input.enqueue(stimulus)

          // Create random input stall delay by inserting steps before enqueue
          makeDelay(inputDelayRand) { delay =>
            dut.clock.step(delay)
          }
        }
      }.fork {
        for (expected <- resultSeq) {
          dut.output.expectDequeue(expected)

          // Create random amount of backpressure, by adding steps before next dequeue
          makeDelay(outputDelayRand) { delay =>
            dut.clock.step(delay)
          }
        }
      }.join()

      // returns number of cycles for this test
      dut.cycles.peek().litValue().toInt
    }

    // test is a little naive but demonstrates that as stalls are introduced the number
    // of cycles for the run grows.
    test(new DecoupledGcd(16)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val cyclesTaken = Seq(
        runTest(dut),
        runTest(dut, inputDelayRand = 7),
        runTest(dut, outputDelayRand = 11),
        runTest(dut, inputDelayRand = 13, outputDelayRand = 17),
        runTest(dut, inputDelayRand = 23, outputDelayRand = 19)
      )
      cyclesTaken.head should be > 600
      cyclesTaken(1) should be > cyclesTaken.head
      cyclesTaken(2) should be > cyclesTaken.head
      cyclesTaken(3) should be > cyclesTaken.head
      cyclesTaken(4) should be > cyclesTaken.head
    }
  }
}