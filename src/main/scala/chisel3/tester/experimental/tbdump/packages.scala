// See LICENSE for license details.

package chisel3.tester.experimental.tbdump

import chisel3._
import chisel3.util._
import chisel3.tester._
import chisel3.tester.experimental.tbdump.util._

import scala.language.implicitConversions
import chisel3.tester.internal._
import chisel3.experimental.{DataMirror, Direction, FixedPoint}

class NotLiteralException(message: String) extends Exception(message)
class LiteralTypeException(message: String) extends Exception(message)
class UnpokeableException(message: String) extends Exception(message)
class UnsupportedOperationException(message: String) extends Exception(message)

class ClockResolutionException(message: String) extends Exception(message)

/** Basic interfaces and implicit conversions for testers2
  */

package object tester{
    import chisel3.internal.firrtl.{LitArg, ULit, SLit}
    implicit class testableData[T <: Data](x: T) {

      protected def pokeBits(signal: Bits, value: BigInt)(implicit tb: Option[TbBase]): Unit = {
        if (DataMirror.directionOf(signal) != Direction.Input) {
          throw new UnpokeableException("Cannot only poke inputs")
        }
        Context().backend.pokeBits(signal, value)
        pokePrint(signal, value)
      }

      protected def expectBits(signal: Bits, value: BigInt, message: Option[String], stale: Boolean)(implicit tb: Option[TbBase]): Unit = {
        Context().backend.expectBits(signal, value, message, stale)
        expectPrint(signal, value, message)
      }

      protected def peekBits(signal: Bits, stale: Boolean)(implicit tb: Option[TbBase]): BigInt = {
        val res = Context().backend.peekBits(signal, stale)
        res
      }

      def poke(value: T)(implicit tb: Option[TbBase]): Unit = (x, value) match {
        case (x: Bool, value: Bool) => pokeBits(x, value.litValue)
        // TODO can't happen because of type parameterization
        case (x: Bool, value: Bits) => throw new LiteralTypeException(s"can only poke signals of type Bool with Bool value")
        case (x: Bits, value: UInt) => pokeBits(x, value.litValue)
        case (x: SInt, value: SInt) => pokeBits(x, value.litValue)
        // TODO can't happen because of type parameterization
        case (x: Bits, value: SInt) => throw new LiteralTypeException(s"can only poke SInt value into signals of type SInt")
        case (x: FixedPoint, value: FixedPoint) => {
          require(x.binaryPoint == value.binaryPoint, "binary point mismatch")
          pokeBits(x, value.litValue)
        }
        case (x: Bundle, value: Bundle) => {
          // TODO: chisel needs to expose typeEquivalent
          require(x.elements.keys == value.elements.keys) // TODO: this discards type data =(
          (x.elements zip value.elements) foreach { case ((_, x), (_, value)) =>
            x.poke(value)
          }
        }
        case x => throw new LiteralTypeException(s"don't know how to poke $x")
        // TODO: aggregate types
      }

      protected def peekWithStale(stale: Boolean)(implicit tb: Option[TbBase]): T = x match {
        case (x: Bool) => peekBits(x, stale) match {
          case x: BigInt if x == 0 => false.B.asInstanceOf[T]
          case x: BigInt if x == 1 => true.B.asInstanceOf[T]
          case x => throw new LiteralTypeException(s"peeked Bool with value $x not 0 or 1")
        }
        case (x: UInt) => peekBits(x, stale).asUInt(x.width).asInstanceOf[T]
        case (x: SInt) => peekBits(x, stale).asUInt(x.width).asInstanceOf[T]
        case (x: FixedPoint) => {
          val multiplier = math.pow(2, x.binaryPoint.get)
          (peekBits(x, stale).toDouble / multiplier).F(x.binaryPoint).asInstanceOf[T]
        }
        case x => throw new LiteralTypeException(s"don't know how to peek $x")
      }

      def peek()(implicit tb: Option[TbBase]): T = peekWithStale(false)

      protected def expectWithStale(value: T, message: Option[String], stale: Boolean)(implicit tb: Option[TbBase]): Unit = (x, value) match {
        case (x: Bool, value: Bool) => expectBits(x, value.litValue, message, stale)
        // TODO can't happen because of type paramterization
        case (x: Bool, value: Bits) => throw new LiteralTypeException(s"cannot expect non-Bool value $value from Bool IO $x")
        case (x: Bits, value: UInt) => expectBits(x, value.litValue, message, stale)
        case (x: SInt, value: SInt) => expectBits(x, value.litValue, message, stale)
        // TODO can't happen because of type paramterization
        case (x: Bits, value: SInt) => throw new LiteralTypeException(s"cannot expect non-SInt value $value from SInt IO $x")
        case (x: FixedPoint, value: FixedPoint) => {
          require(x.binaryPoint == value.binaryPoint, s"binary point mismatch between value $value from IO $x")
          expectBits(x, value.litValue, message, stale)
        }
        case (x: Bundle, value: Bundle) => {
          // TODO: chisel needs to expose typeEquivalent
          require(x.elements.keys == value.elements.keys) // TODO: this discards type data =(
          (x.elements zip value.elements) foreach { case ((_, x), (_, value)) =>
            x.expectWithStale(value, message, stale)
          }
        }
        case x => throw new LiteralTypeException(s"don't know how to expect $x")
        // TODO: aggregate types
      }

      def expect(value: T)(implicit tb: Option[TbBase]): Unit = expectWithStale(value, None, false)

      def expect(value: T, message: String)(implicit tb: Option[TbBase]): Unit = expectWithStale(value, Some(message), false)

      /** @return the single clock that drives the source of this signal.
        * @throws ClockResolutionException if sources of this signal have more than one, or zero clocks
        * @throws ClockResolutionException if sinks of this signal have an associated clock
        */
      def getSourceClock(): Clock = {
        Context().backend.getSourceClocks(x).toList match {
          case clock :: Nil => clock
          case clocks => throw new ClockResolutionException(s"number of source clocks for $x is not one: $clocks")
        }
      }

      def getSinkClock(): Clock = {
        Context().backend.getSinkClocks(x).toList match {
          case clock :: Nil => clock
          case clocks => throw new ClockResolutionException(s"number of sink clocks for $x is not one: $clocks")
        }
      }

      protected def waitBits(t: Bits, value: BigInt, clk: Clock)(implicit tb: Option[TbBase]): Unit = {
         waitPrint(t, value)
         while(Context().backend.peekBits(t, false) != value){
           Context().backend.step(clk, 1)
         }
      }

      def waitAndStep(value: T, clk: Clock)(implicit tb: Option[TbBase]): Unit = (x, value) match {
        case (x: Bool, value: Bool) => waitBits(x, value.litValue,clk)
        // TODO can't happen because of type paramterization
        case (x: Bool, value: Bits) => throw new LiteralTypeException(s"cannot expect non-Bool value $value from Bool IO $x")
        case (x: Bits, value: UInt) => waitBits(x, value.litValue,clk)
        case (x: SInt, value: SInt) => waitBits(x, value.litValue,clk)
        // TODO can't happen because of type paramterization
        case (x: Bits, value: SInt) => throw new LiteralTypeException(s"cannot expect non-SInt value $value from SInt IO $x")
        case (x: FixedPoint, value: FixedPoint) => {
          require(x.binaryPoint == value.binaryPoint, s"binary point mismatch between value $value from IO $x")
          waitBits(x, value.litValue,clk)
        }
        case (x: Bundle, value: Bundle) => {
          // TODO: chisel needs to expose typeEquivalent
          require(x.elements.keys == value.elements.keys) // TODO: this discards type data =(
          (x.elements zip value.elements) foreach { case ((_, x), (_, value)) =>
            x.waitAndStep(value, clk)
          }
        }
        case x => throw new LiteralTypeException(s"don't know how to expect $x")
        // TODO: aggregate types
      }
    }

    implicit class testableClock(x: Clock) {
      def setTimeout(cycles: Int)(implicit tb: Option[TbBase]): Unit = {
        timeoutPrint(cycles)
        Context().backend.setTimeout(x, cycles)
      }

      def step(cycles: Int = 1)(implicit tb: Option[TbBase]): Unit = {
        stepPrint(cycles)
        Context().backend.step(x, cycles)
      }
    }

    object fork extends ForkBuilder(None, None, Seq())

    // TODO: call-by-name doesn't work with varargs, is there a better way to do this?
    def parallel(run1: => Unit, run2: => Unit)(implicit tb: Option[TbBase]): Unit = {
      fork {
        run1
      }
        .fork {
          run2
        }
        .join
    }

    def timescope(contents: => Unit)(implicit tb: Option[TbBase]): Unit = {
      timescopeBeginPrint
      Context().backend.doTimescope(() => contents)
      timescopeEndPrint
    }

    object TestInstance {
      def setVar(key: Any, value: Any): Unit = {
        Context().backend.setVar(key, value)
      }

      def getVar(key: Any): Option[Any] = {
        Context().backend.getVar(key)
      }
    }

    /** Provides clock-resolution-specific abstractions on top of getVar/setVar.
      * For library builders, not top-level test writers.
      */
    object ClockResolutionUtils {
      def setClock(driverKey: Any, wire: Data, clock: Clock): Unit = {
        TestInstance.setVar((driverKey, wire), clock)
      }

      def getClock(driverKey: Any, wire: Data, defaultClock: => Clock): Clock = {
        TestInstance.getVar((driverKey, wire)) match {
          case None =>
            val clock: Clock = defaultClock
            setClock(driverKey, wire, clock)
            clock
          case Some(clock: Clock) => clock
          case Some(other) => throw new ClockResolutionException(s"$other is not a clock")
        }
      }
    }

    def finish(implicit tb: Option[TbBase]): Unit = {
      finishPrint
    }

    implicit def decoupledToDriver[T <: Data](x: DecoupledIO[T]) = new DecoupledDriver(x)

    implicit def validToDriver[T <: Data](x: ValidIO[T]) = new ValidDriver(x)
}
