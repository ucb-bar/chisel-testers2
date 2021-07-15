// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator

import scala.util.Random

/** Compliance tests for the `peek` and `poke` functions of the [[SimulatorContext]] interface.  */
abstract class PeekPokeCompliance(sim: Simulator) extends ComplianceTest(sim) {
  behavior of sim.name



  private def comb(f: String, bits: Int): String =
    s"""circuit test:
       |  module test:
       |    input a : UInt<$bits>
       |    input b : UInt<$bits>
       |    output o : UInt<$bits>
       |    o <= $f
       |""".stripMargin

  it should "propagate values through the circuit" in {
    val rand = new Random(0)
    val ctx = load(comb("a", 43))
    val value = BigInt(43, rand)
    ctx.poke("a", value)
    assert(ctx.peek("b") == value)
  }

}
