// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator

import org.scalatest.Tag

import scala.util.Random

/** Compliance tests for the `peek` and `poke` functions of the [[SimulatorContext]] interface.  */
abstract class PeekPokeCompliance(sim: Simulator, tag: Tag = DefaultTag) extends ComplianceTest(sim, tag) {
  behavior of sim.name

  private val bufferedInverter =
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

  it should "poke inputs and peek outputs" taggedAs(tag) in {
    val dut = load(bufferedInverter)

    dut.poke("in", 1)
    dut.step()
    assert(dut.peek("out") == 0)

    dut.poke("in", 0)
    dut.step()
    assert(dut.peek("out") == 1)

    dut.finish()
  }

  it should "peek inputs as well as outputs" taggedAs(tag) in {
    val dut = load(bufferedInverter)

    dut.poke("in", 1)
    dut.step()
    assert(dut.peek("out") == 0)
    // peek an input
    assert(dut.peek("in") == 1)

    dut.finish()
  }

  private def comb(f: String, bits: Int): String =
    s"""circuit test:
       |  module test:
       |    input a : UInt<$bits>
       |    input b : UInt<$bits>
       |    output o : UInt<$bits>
       |    o <= $f
       |""".stripMargin

  it should "propagate values through the circuit" taggedAs(tag) in {
    val rand = new Random(0)
    val dut = load(comb("a", 43))

    val value = BigInt(43, rand)
    dut.poke("a", value)
    assert(dut.peek("o") == value)

    dut.finish()
  }

}
