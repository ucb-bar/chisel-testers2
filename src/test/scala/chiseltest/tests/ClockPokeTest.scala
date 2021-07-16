// SPDX-License-Identifier: Apache-2.0

package chiseltest.tests

import org.scalatest._

import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.experimental.UncheckedClockPoke._
import org.scalatest.flatspec.AnyFlatSpec

class ClockPokeTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Testers2 with a clock input"

  it should "work as expected" ignore { // TODO: re-add multi clock support
    test(new Module {
      val inClock = IO(Input(Clock()))
      val out = IO(Output(UInt(8.W)))

      withClock(inClock) {
        out := Counter(true.B, 8)._1
      }
    }) { c =>
      c.inClock.low()
      c.out.expect(0.U)

      // Main clock should do nothing
      c.clock.step()
      c.out.expect(0.U)
      c.clock.step()
      c.out.expect(0.U)

      // Output should advance on rising edge, even without main clock edge
      c.inClock.high()
      c.out.expect(1.U)

      // Repeated high should do nothing
      c.inClock.high()
      c.out.expect(1.U)

      // and again
      c.inClock.low()
      c.out.expect(1.U)
      c.inClock.high()
      c.out.expect(2.U)
    }
  }
}
