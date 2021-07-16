// SPDX-License-Identifier: Apache-2.0

package chiseltest

import chiseltest.internal._
import chisel3.Module
import chiseltest.simulator.SimulatorAnnotation
import firrtl.AnnotationSeq

package object defaults {
  // TODO: I think we need a way to specify global defaults, e.g. to say 'run all tests under verilator'

  /** Creates a DefaultTester from the desired backend
    *
    * @param dutGen          device under test
    * @param annotationSeq   initial annotations
    * @tparam T              dut type
    * @return                a backend for the dut type
    */
  def createDefaultTester[T <: Module](dutGen: () => T, annotationSeq: AnnotationSeq): BackendInstance[T] = {
    // if there is not backend specified, use treadle
    val hasSimulator = annotationSeq.exists(_.isInstanceOf[SimulatorAnnotation])
    val annos: AnnotationSeq = if (hasSimulator) { annotationSeq }
    else { TreadleBackendAnnotation +: annotationSeq }
    BackendExecutive.start(dutGen, annos)
  }
}
