// SPDX-License-Identifier: Apache-2.0

import scala.language.implicitConversions
import chiseltest.internal._
import chisel3._
import chisel3.experimental.{DataMirror, Direction, EnumType, FixedPoint, Interval}
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.VecLiterals._
import chisel3.util._

/** Basic interfaces and implicit conversions for testers2
  */
package object chiseltest {
  import chisel3.internal.firrtl.{LitArg, SLit, ULit}

  implicit class testableRecord[T <: Record](x: T) {

    /** Poke the given signal with a [[Record.litValue()]]
      * Literals of this Record can be instantiated with
      * {{{
      *   someRecord.Lit(_.elements("foo") -> 4.U)
      * }}}
      * `pokePartial` will only poke [[Input]] signals of `x`,
      * and elements of `x` which contain no literal will be ignored.
      */
    def pokePartial(value: T): Unit = {
      require(DataMirror.checkTypeEquivalence(x, value), s"Record type mismatch")
      x.elements.filter { case (k, v) =>
        DataMirror.directionOf(v) != ActualDirection.Output && {
          value.elements(k) match {
            case _:    Record => true
            case data: Data   => data.isLit()
          }
        }
      }.foreach { case (k, v) =>
        v match {
          case record: Record => record.pokePartial(value.elements(k).asInstanceOf[Record])
          case data:   Data   => data.poke(value.elements(k))
        }
      }
    }

    /** Check the given signal with a [[Record.litValue()]];
      * elements of `x` which contain no literal will be ignored.
      */
    def expectPartial(value: T): Unit = {
      require(DataMirror.checkTypeEquivalence(x, value), s"Record type mismatch")
      x.elements.filter { case (k, v) =>
        value.elements(k) match {
          case _: Record => true
          case d: Data   => d.isLit()
        }
      }.foreach { case (k, v) =>
        v match {
          case record: Record => record.expectPartial(value.elements(k).asInstanceOf[Record])
          case data:   Data   => data.expect(value.elements(k))
        }
      }
    }
  }

  implicit class testableVec[T <: Vec[_]](x: T) {

    /** Poke the given signal with a [[Vec.litValue()]]
      * Literals of this Record can be instantiated with
      * {{{
      *   someVec.Lit(_.elements("foo") -> 4.U)
      * }}}
      * `pokePartial` will only poke [[Input]] signals of `x`,
      * and elements of `x` which contain no literal will be ignored.
      */
    def pokePartial(value: T): Unit = {
      require(DataMirror.checkTypeEquivalence(x, value), s"Vec type mismatch")
      x.getElements.zipWithIndex.filter { case (v, index) =>
        DataMirror.directionOf(v) != ActualDirection.Output && {
          value.getElements(index) match {
            case _:    T    => true
            case data: Data => data.isLit()
          }
        }
      }.foreach { case (v, index) =>
        v match {
          case vec:  T    => vec.pokePartial(value.getElements(index).asInstanceOf[T])
          case data: Data => data.poke(value.getElements(index))
        }
      }
    }

    /** Check the given signal with a [[Vec.litValue()]];
      * elements of `x` which contain no literal will be ignored.
      */
    def expectPartial(value: T): Unit = {
      require(DataMirror.checkTypeEquivalence(x, value), s"Vec type mismatch")
      x.getElements.zipWithIndex.filter { case (v, index) =>
        value.getElements(index) match {
          case _: T    => true
          case d: Data => d.isLit()
        }
      }.foreach { case (v, index) =>
        v match {
          case vec:  T    => vec.expectPartial(value.getElements(index).asInstanceOf[T])
          case data: Data => data.expect(value.getElements(index))
        }
      }
    }
  }

  private object BitsDecoders {
    import chisel3.internal.firrtl.{BinaryPoint, KnownBinaryPoint, UnknownBinaryPoint}

    def boolBitsToString(bits: BigInt): String = (bits != 0).toString

    def fixedToString(binaryPoint: BinaryPoint): BigInt => String = {
      def inner(bits: BigInt): String = {
        binaryPoint match {
          case KnownBinaryPoint(binaryPoint) =>
            val bpInteger = 1 << binaryPoint
            (bits.toFloat / bpInteger).toString
          case UnknownBinaryPoint => "[unknown binary point]"
        }
      }
      inner
    }

    def enumToString(record: EnumType): BigInt => String = {
      def inner(bits: BigInt): String = "[unimplemented enum decode]"
      inner
    }
  }

  implicit class testableData[T <: Data](x: T) {
    protected def pokeBits(signal: Data, value: BigInt): Unit = {
      if (DataMirror.directionOf(signal) != Direction.Input) {
        throw new UnpokeableException("Can only poke inputs")
      }
      // Some backends can behave incorrectly if too many bits are poked into their inputs
      val maskedValue = value & ((BigInt(1) << signal.widthOption.get) - 1)
      Context().backend.pokeBits(signal, maskedValue)
    }

    def poke(value: T): Unit = (x, value) match {
      case (x: Bool, value: Bool) => pokeBits(x, value.litValue)
      // TODO can't happen because of type parameterization
      case (x: Bool, value: Bits) =>
        throw new LiteralTypeException(s"can only poke signals of type Bool with Bool value")
      case (x: Bits, value: UInt) => pokeBits(x, value.litValue)
      case (x: SInt, value: SInt) => pokeBits(x, value.litValue)
      // TODO can't happen because of type parameterization
      case (x: Bits, value: SInt) =>
        throw new LiteralTypeException(s"can only poke SInt value into signals of type SInt")
      case (x: FixedPoint, value: FixedPoint) => {
        require(x.binaryPoint == value.binaryPoint, "binary point mismatch")
        pokeBits(x, value.litValue)
      }
      case (x: Interval, value: Interval) =>
        require(x.binaryPoint == value.binaryPoint, "binary point mismatch")
        pokeBits(x, value.litValue)
      case (x: Record, value: Record) => {
        require(DataMirror.checkTypeEquivalence(x, value), s"Record type mismatch")
        (x.elements.zip(value.elements)).foreach { case ((_, x), (_, value)) =>
          x.poke(value)
        }
      }
      case (x: Vec[_], value: Vec[_]) => {
        require(DataMirror.checkTypeEquivalence(x, value), s"Vec type mismatch")
        (x.getElements.zip(value.getElements)).foreach { case (x, value) =>
          x.poke(value)
        }
      }
      case (x: EnumType, value: EnumType) => {
        require(DataMirror.checkTypeEquivalence(x, value), s"EnumType mismatch")
        pokeBits(x, value.litValue)
      }
      case x => throw new LiteralTypeException(s"don't know how to poke $x")
      // TODO: aggregate types
    }

    protected def peekWithStale(stale: Boolean): T = x match {
      case (x: Bool) =>
        Context().backend.peekBits(x, stale) match {
          case x: BigInt if x == 0 => false.B.asInstanceOf[T]
          case x: BigInt if x == 1 => true.B.asInstanceOf[T]
          case x => throw new LiteralTypeException(s"peeked Bool with value $x not 0 or 1")
        }
      case (x: UInt) => Context().backend.peekBits(x, stale).asUInt(DataMirror.widthOf(x)).asInstanceOf[T]
      case (x: SInt) => Context().backend.peekBits(x, stale).asSInt(DataMirror.widthOf(x)).asInstanceOf[T]
      case (x: FixedPoint) => {
        val multiplier = BigDecimal(2).pow(x.binaryPoint.get)
        (BigDecimal(Context().backend.peekBits(x, stale)) / multiplier).F(x.binaryPoint).asInstanceOf[T]
      }
      case x: Interval =>
        Context().backend.peekBits(x, stale).I(x.binaryPoint).asInstanceOf[T]
      case (x: Record) => {
        val elementValueFns = x.elements.map { case (name: String, elt: Data) =>
          (y: Record) => (y.elements(name), elt.peekWithStale(stale))
        }.toSeq
        chiselTypeOf(x).Lit(elementValueFns: _*).asInstanceOf[T]
      }
      case (x: Vec[_]) =>
        val elementValueFns = x.getElements.map { case elt: Data =>
          elt.peekWithStale(stale)
        }
        Vec.Lit(elementValueFns: _*).asInstanceOf[T]
      case (x: EnumType) => {
        throw new NotImplementedError(s"peeking enums ($x) not yet supported, need programmatic enum construction")
      }
      case x => throw new LiteralTypeException(s"don't know how to peek $x")
    }

    def peek(): T = peekWithStale(false)

    protected def expectWithStale(value: T, message: Option[String], stale: Boolean): Unit = (x, value) match {
      case (x: Bool, value: Bool) =>
        Context().backend.expectBits(x, value.litValue, message, Some(BitsDecoders.boolBitsToString), stale)
      // TODO can't happen because of type parameterization
      case (x: Bool, value: Bits) =>
        throw new LiteralTypeException(s"cannot expect non-Bool value $value from Bool IO $x")
      case (x: Bits, value: UInt) => Context().backend.expectBits(x, value.litValue, message, None, stale)
      case (x: SInt, value: SInt) => Context().backend.expectBits(x, value.litValue, message, None, stale)
      // TODO can't happen because of type parameterization
      case (x: Bits, value: SInt) =>
        throw new LiteralTypeException(s"cannot expect non-SInt value $value from SInt IO $x")
      case (x: FixedPoint, value: FixedPoint) => {
        require(x.binaryPoint == value.binaryPoint, "binary point mismatch")
        Context().backend.expectBits(x, value.litValue, message, Some(BitsDecoders.fixedToString(x.binaryPoint)), stale)
      }
      case (x: Interval, value: Interval) =>
        require(x.binaryPoint == value.binaryPoint, "binary point mismatch")
        Context().backend.expectBits(x, value.litValue, message, Some(BitsDecoders.fixedToString(x.binaryPoint)), stale)
      case (x: Record, value: Record) => {
        require(DataMirror.checkTypeEquivalence(x, value), s"Record type mismatch")
        (x.elements.zip(value.elements)).foreach { case ((_, x), (_, value)) =>
          x.expectWithStale(value, message, stale)
        }
      }
      case (x: Vec[_], value: Vec[_]) => {
        require(DataMirror.checkTypeEquivalence(x, value), s"Vec type mismatch")
        (x.getElements.zip(value.getElements)).foreach { case (x, value) =>
          x.expectWithStale(value, message, stale)
        }
      }
      case (x: EnumType, value: EnumType) => {
        require(DataMirror.checkTypeEquivalence(x, value), s"EnumType mismatch")
        Context().backend.expectBits(x, value.litValue, message, Some(BitsDecoders.enumToString(x)), stale)
      }
      case x => throw new LiteralTypeException(s"don't know how to expect $x")
      // TODO: aggregate types
    }

    def expect(value: T): Unit = expectWithStale(value, None, false)
    def expect(value: T, message: String): Unit = expectWithStale(value, Some(message), false)

    /** @return the single clock that drives the source of this signal.
      * @throws ClockResolutionException if sources of this signal have more than one, or zero clocks
      * @throws ClockResolutionException if sinks of this signal have an associated clock
      */
    def getSourceClock(): Clock = {
      Context().backend.getSourceClocks(x).toList match {
        case clock :: Nil => clock
        case clocks       => throw new ClockResolutionException(s"number of source clocks for $x is not one: $clocks")
      }
    }

    def getSinkClock(): Clock = {
      Context().backend.getSinkClocks(x).toList match {
        case clock :: Nil => clock
        case clocks       => throw new ClockResolutionException(s"number of sink clocks for $x is not one: $clocks")
      }
    }
  }

  implicit class testableClock(x: Clock) {
    def setTimeout(cycles: Int): Unit = {
      Context().backend.setTimeout(x, cycles)
    }

    def step(cycles: Int = 1): Unit = {
      Context().backend.step(x, cycles)
    }
  }

  object fork extends ForkBuilder(None, None, Seq())

  // TODO: call-by-name doesn't work with varargs, is there a better way to do this?
  def parallel(run1: => Unit, run2: => Unit): Unit = {
    fork { run1 }.fork { run2 }.join
  }

  def timescope(contents: => Unit): Unit = {
    Context().backend.doTimescope(() => contents)
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

  implicit def decoupledToDriver[T <: Data](x: ReadyValidIO[T]) = new DecoupledDriver(x)

  implicit def validToDriver[T <: Data](x: ValidIO[T]) = new ValidDriver(x)

  // expose public flags
  val VerilatorBackendAnnotation = simulator.VerilatorBackendAnnotation
  val TreadleBackendAnnotation = simulator.TreadleBackendAnnotation
  val WriteVcdAnnotation = simulator.WriteVcdAnnotation
}
