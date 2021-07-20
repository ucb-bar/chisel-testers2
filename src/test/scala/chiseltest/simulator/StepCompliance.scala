// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator

import org.scalatest.Tag

/** Ensures that the `step` function works correctly, no or one clock. */
abstract class StepCompliance(sim: Simulator, tag: Tag = DefaultTag) extends ComplianceTest(sim, tag) {
  private val base =
    """circuit test:
      |  module test:
      |    input clock: Clock
      |    input in: UInt<1>
      |    output out: UInt<1>
      |
      |    reg r : UInt<1>, clock with :
      |      reset => (UInt<1>(0), r)
      |    r <= not(in)
      |    out <= r
      |
      |""".stripMargin

  it should "be able to step a simple circuit with a single clock" taggedAs(tag) in {
    val dut = load(base)
    // note: the value of the register at the beginning of the simulation is undefined
    dut.poke("in", 0)
    dut.step()
    assert(dut.peek("out") == 1)
    // changing the input should _not_ immediately affect the output!
    dut.poke("in", 1)
    assert(dut.peek("out") == 1)
    // after a step, the output changes
    dut.step()
    assert(dut.peek("out") == 0)
    dut.finish()
  }

  private val combCircuit =
    """circuit test:
      |  module test:
      |    input in: UInt<1>
      |    output out: UInt<1>
      |
      |    out <= not(in)
      |""".stripMargin

  it should "throw an exception when trying to step a circuit with no clock" taggedAs(tag) in {
    val dut = load(combCircuit)
    assertThrows[NoClockException] {
      dut.step()
    }
    dut.finish()
  }

  it should "support stepping multiple cycles" taggedAs(tag) in {
    val src = CounterGen(List("clock"))
    val dut = load(src) // a counter with the standard "clock" clock
    dut.poke("reset", 1)
    dut.step()
    dut.poke("reset", 0)

    assert(dut.peek("clock_count") == 0)
    dut.step()
    assert(dut.peek("clock_count") == 1)
    dut.step(20)
    assert(dut.peek("clock_count") == 21)
    dut.step(1234)
    assert(dut.peek("clock_count") == 1234 + 20 + 1)

    dut.finish()
  }

  it should "support stepping a clock that is not named 'clock'" taggedAs(tag) in {
    val clock = "test123"
    val dut = load(CounterGen(List(clock))) // a counter with a single clock with non standard name
    dut.poke("reset", 1)
    dut.step()
    dut.poke("reset", 0)

    assert(dut.peek(clock + "_count") == 0)
    dut.step()
    assert(dut.peek(clock + "_count") == 1)
    dut.step(20)
    assert(dut.peek(clock + "_count") == 21)
    dut.step(1234)
    assert(dut.peek(clock + "_count") == 1234 + 20 + 1)

    dut.finish()
  }


}

private object CounterGen {
  def apply(clocks: List[String]): String = {
    List("circuit test:", "  module test:", "    input reset: UInt<1>") ++
      clocks.map(c => s"    input $c: Clock") ++
      clocks.map(c => s"    output ${c}_count: UInt<32>") ++
      List("") ++
      clocks.flatMap { c =>
        List(s"    reg ${c}_r : UInt<32>, $c with :", s"      reset => (reset, UInt(0))",
          s"    ${c}_r <= add(${c}_r, UInt(1))", s"    ${c}_count <= ${c}_r")
      }
  }.mkString("\n") + "\n"
}

/** Ensures that the `step` function works correctly for circuits with multiple clocks */
abstract class StepMultiClockCompliance(sim: Simulator, tag: Tag = DefaultTag) extends ComplianceTest(sim, tag) {
  // TODO add multi-clock support
}
