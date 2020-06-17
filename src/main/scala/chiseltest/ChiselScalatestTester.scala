// See LICENSE for license details.

package chiseltest

import chisel3.MultiIOModule
import chisel3.stage.ChiselGeneratorAnnotation
import chiseltest.internal.ExpectExceptionsAnnotation
import chiseltest.stage.{ChiselTestStage, TestFunctionAnnotation}
import firrtl.AnnotationSeq
import org.scalatest._
import org.scalatest.exceptions.TestFailedException

import scala.util.DynamicVariable

trait ChiselScalatestTester extends Assertions with TestSuiteMixin {
  this: TestSuite =>

  class TestBuilder[T <: MultiIOModule](val dutGen: () => T, val annotationSeq: AnnotationSeq, val flags: Array[String]) {
    def withAnnotations(newAnnotationSeq: AnnotationSeq): TestBuilder[T] =
      new TestBuilder[T](dutGen, annotationSeq ++ newAnnotationSeq, flags)

    def withFlags(newFlags: Array[String]): ChiselScalatestTester#TestBuilder[T] =
      new TestBuilder[T](dutGen, annotationSeq, flags ++ newFlags)

    def apply(testFn: T => Unit): Unit = (new ChiselTestStage).execute(flags, Seq(
      TestFunctionAnnotation(testFn),
      new ChiselGeneratorAnnotation(dutGen)
    ) ++ annotationSeq).collectFirst {
      case ExpectExceptionsAnnotation(expects) => if (expects.nonEmpty) throw new TestFailedException(expects.map(_.message).mkString("\n"), 0)
    }
  }

  // Provide test fixture data as part of 'global' context during test runs
  protected var scalaTestContext = new DynamicVariable[Option[NoArgTest]](None)

  abstract override def withFixture(test: NoArgTest): Outcome = {
    require(scalaTestContext.value.isEmpty)
    scalaTestContext.withValue(Some(test)) {
      super.withFixture(test)
    }
  }

  /**
    * Constructs a unit test harness for the Chisel Module generated by dutGen.
    * General use looks like
    * {{{
    *   test(new PlusOne) { c =>
    *     // body of the unit test, c is a a reference
    *     c.io.input.poke(1.U)
    *     c.io.output.expect(2.U)
    *   }
    * }}}
    *
    * If you need to add options to this unit test you can tack on .withAnnotations modifier
    * or a .withFlags modifier. These modifiers can be used together.
    * You must add `import chisel3.tester.experimental.TestOptionBuilder._` to use .withAnnotations
    *
    * For example:
    * {{{
    *   test(new TestModule).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
    *     // body of the unit test
    *   }
    * }}}
    *
    * @see src/test/scala/chisel3/tests/OptionsBackwardCompatibilityTest for examples
    * @note This API is experimental and forward compatibility is not yet guaranteed
    * @param dutGen A generator of a Chisel  Module
    * @tparam T The DUT type, must be a subclass of MultiIOModule
    * @return
    */
  def test[T <: MultiIOModule](dutGen: => T): TestBuilder[T] = {
    new TestBuilder(() => dutGen, Seq.empty, Array.empty)
  }
}
