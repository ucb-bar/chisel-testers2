// SPDX-License-Identifier: Apache-2.0

package chiseltest.experimental

import chisel3.stage.ChiselCli
import chiseltest.simulator.{TreadleBackendAnnotation, VerilatorBackendAnnotation, WriteVcdAnnotation}
import firrtl.options.Shell
import firrtl.stage.FirrtlCli

trait ChiselTestCli extends ChiselCli with FirrtlCli {
  this: Shell =>

  parser.note("ChiselTest Options")

  Seq(
    VerilatorBackendAnnotation,
    TreadleBackendAnnotation,
    WriteVcdAnnotation
  ).foreach(_.addOptions(parser))
}

class ChiselTestShell extends Shell("ChiselTest") with ChiselTestCli
