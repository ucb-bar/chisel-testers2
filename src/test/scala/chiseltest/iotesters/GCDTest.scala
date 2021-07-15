
package chiseltest.iotesters

import treadle.chronometry.Timer

class IoTestersGcd(gcd: DecoupledGcd, testValues: Seq[(Int, Int, Int)]) extends PeekPokeTester(gcd) {
  reset(2)
  poke(gcd.output.ready, 1)
  for ((i, j, expected) <- testValues) {
    poke(gcd.input.bits.value1, i)
    poke(gcd.input.bits.value2, j)
    poke(gcd.input.valid, 1)
    step(1)

    while (peek(gcd.output.valid) == 0) {
      step(1)
    }
    expect(gcd.output.bits.gcd, expected, s"($i,$j) expected: $expected, got: ${peek(gcd.output.bits.gcd)}")
    expect(gcd.output.valid, 1)
  }
}


object GcdRegression {
  def main(args: Array[String]): Unit = {
    val t = new Timer
    val Repetitions = 6

    def computeGcd(a: Int, b: Int): Int = {
      var x = a
      var y = b
      var depth = 1
      while (y > 0) {
        if (x > y) {
          x -= y
        }
        else {
          y -= x
        }
        depth += 1
      }
      x
    }

    val testValues = (for {x <- 2 to 100; y <- 2 to 100} yield (x, y, computeGcd(x, y)))

    val backendName = "verilator" // if (args.nonEmpty) { "verilator" } else { "treadle" }

    Driver.execute(Array(
      "--target-dir", "test_run_dir/iotesters_gcd",
      "--top-name", "iotesters_gcd",
      "--backend-name", backendName,
      // "--generate-vcd-output", "on",
      //      "-tiwv"
    ), () => new DecoupledGcd(bitWidth = 60)) { dut =>
      t("iotesters-gcd") {
        new IoTestersGcd(dut, testValues)
      }
      t("iotesters-gcd") {
        new IoTestersGcd(dut, testValues)
      }
      t("iotesters-gcd") {
        new IoTestersGcd(dut, testValues)
      }

      t("iotesters-gcd") {
        new IoTestersGcd(dut, testValues)
      }
      t("iotesters-gcd") {
        new IoTestersGcd(dut, testValues)
      }
      t("iotesters-gcd") {
        new IoTestersGcd(dut, testValues)
      }
    }

    println(t.report())
  }
}
