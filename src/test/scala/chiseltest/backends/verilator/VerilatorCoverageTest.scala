// SPDX-License-Identifier: Apache-2.0

package chiseltest.backends.verilator

import chiseltest.VerilatorBackendAnnotation
import chiseltest.coverage.SimulatorCoverageTest

class VerilatorCoverageTest extends SimulatorCoverageTest("Verilator", VerilatorBackendAnnotation) {}
