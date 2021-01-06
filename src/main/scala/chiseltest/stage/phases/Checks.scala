// SPDX-License-Identifier: Apache-2.0

package chiseltest.stage.phases

import chiseltest.stage._
import firrtl.AnnotationSeq
import firrtl.options.{OptionsException, Phase}

class Checks extends Phase {
  override def prerequisites = Seq.empty

  override def optionalPrerequisites = Seq.empty

  override def optionalPrerequisiteOf = Seq.empty

  override def invalidates(a: Phase) = false

  def transform(annotations: AnnotationSeq): AnnotationSeq = {
    if (annotations.collectFirst { case TestNameAnnotation(n) => n }.isEmpty)
      throw new OptionsException(s"chiseltest cannot define the test path.")

    if (annotations.collectFirst { case TestFunctionAnnotation(t) => t }.isEmpty)
      throw new OptionsException(s"no test function provided.")

    if (
      annotations.count {
        case _: BackendAnnotation => true
        case _ => false
      } > 1
    )
      throw new OptionsException(s"Only one backend is allowed.")

    annotations
  }
}
