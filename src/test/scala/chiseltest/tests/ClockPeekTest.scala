// SPDX-License-Identifier: Apache-2.0

package chiseltest.tests

import org.scalatest._

import chisel3._
import chiseltest._
import chiseltest.experimental.UncheckedClockPoke._
import chiseltest.experimental.UncheckedClockPeek._
import org.scalatest.flatspec.AnyFlatSpec

class ClockPeekTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Testers2 with clock peeking"

  it should "work as expected" ignore { // TODO: re-enable support for multi-clock
    test(new Module {
      val inClock = IO(Input(Clock()))
      val outClock = IO(Output(Clock()))
      outClock := inClock
    }) { c =>
      c.inClock.low()
      // Main clock should do nothing
      c.clock.step()
      assert(c.inClock.peekClock() == false)
      assert(c.outClock.peekClock() == false)
      c.inClock.high()
      c.clock.step()
      assert(c.inClock.peekClock() == true)
      assert(c.outClock.peekClock() == true)
    }
  }
}
