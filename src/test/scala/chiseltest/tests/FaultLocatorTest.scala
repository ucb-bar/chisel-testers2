package chiseltest.tests

import org.scalatest._

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FaultLocatorTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Testers2"

  it should "locate source lines" in {
    intercept[exceptions.TestFailedException] {
      test(new StaticModule(42.U)) { c =>
        c.out.expect(0.U)
      }
    }.failedCodeFileNameAndLineNumberString.get should equal ("FaultLocatorTest.scala:16")
  }

  it should "locate source lines across threads" in {
    val exc = intercept[exceptions.TestFailedException] {
      test(new StaticModule(42.U)) { c =>
        fork {
          c.clock.step()
          c.out.expect(0.U)
        }.join()
      }
    }
    exc.getMessage should include regex ("""\(lines in FaultLocatorTest\.scala:[^\)]*24.*\)""")
  }

  it should "locate source lines in libraries" in {
    val exc = intercept[exceptions.TestFailedException] {
      test(new PassthroughQueue(Bool())) { c =>
        c.out.initSink()
        c.out.setSinkClock(c.clock)

        c.in.valid.poke(true.B)
        c.in.bits.poke(false.B)  // Have this be a data failure only
        c.out.expectDequeueNow(true.B)
      }
    }
    // Only check the filename to avoid this being too brittle as implementation changes
    exc.failedCodeFileNameAndLineNumberString.get should startWith ("DecoupledDriver.scala:")
    exc.getMessage should include regex ("""\(lines in FaultLocatorTest\.scala:[^\)]*41.*\)""")
  }

  it should "locate source lines, even in a different thread" in {
    val exc = intercept[exceptions.TestFailedException] {
      test(new PassthroughQueue(Bool())) { c =>
        c.out.initSink()
        c.out.setSinkClock(c.clock)

        c.in.valid.poke(true.B)
        c.in.bits.poke(false.B)  // Have this be a data failure only
        fork {
          c.out.expectDequeueNow(true.B)
        } .join
      }
    }
    exc.failedCodeFileNameAndLineNumberString.get should startWith ("DecoupledDriver.scala:")
    exc.getMessage should include regex ("""\(lines in FaultLocatorTest\.scala:[^\)]*58.*\)""")
  }
}
