// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator

import org.scalatest.Tag
import org.scalatest.flatspec.AnyFlatSpec


/** To disable tests that require the commercial VCS simulator use the following: `sbt testOnly -- -l RequiresVcs` */
object RequiresVcs extends Tag("RequiresVcs")

class VcsBasicCompliance extends BasicCompliance(VcsSimulator, RequiresVcs)
class VcsStepCompliance extends StepCompliance(VcsSimulator, RequiresVcs)
class VcsPeekPokeCompliance extends PeekPokeCompliance(VcsSimulator, RequiresVcs)

class VcsSpecificTests extends AnyFlatSpec {
  behavior of "Vcs"

  private val sim = VcsSimulator

  it should "print the version that we depend on" taggedAs(RequiresVcs) in {
    val (_, out) = CaptureStdout {
      sim.findVersions
    }
    assert(out.contains("Found Vcs"))
  }
}