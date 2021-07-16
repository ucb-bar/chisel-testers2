// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator

import org.scalatest.flatspec.AnyFlatSpec

class VerilatorBasicCompliance extends BasicCompliance(VerilatorSimulator)
class VerilatorStepCompliance extends StepCompliance(VerilatorSimulator)
class VerilatorPeekPokeCompliance extends PeekPokeCompliance(VerilatorSimulator)

class VerilatorSpecificTests extends AnyFlatSpec {
  behavior of "verilator"

  private val sim = VerilatorSimulator

  it should "print the version that we depend on" in {
    val (_, out) = CaptureStdout {
      sim.findVersions
    }
    assert(out.contains("Found Verilator 4"))
  }
}