package chiseltest.iotesters

import chisel3._
import chisel3.{Aggregate, Bits, Element, InstanceId, Module}
import chisel3.experimental.EnumType
import chisel3.experimental.{FixedPoint, Interval}
import chisel3.internal.firrtl.KnownBinaryPoint
import chiseltest.simulator.SimulatorContext

import scala.collection.immutable.SeqMap
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions
import scala.util.Random
import scala.annotation.implicitNotFound

// A typeclass that defines the types we can poke, peek, or expect from
@implicitNotFound("Cannot peek or poke elements of type ${T}")
trait Pokeable[-T]

object Pokeable {
  implicit object BitsPokeable extends Pokeable[Bits]
  implicit object EnumPokeable extends Pokeable[EnumType]

  trait IsRuntimePokeable // A trait that is applied to elements that were proven to be
  // pokeable at runtime (usually in match statements)
  implicit object RuntimePokeable extends Pokeable[IsRuntimePokeable]

  def unapply(elem: Element): Option[Element with IsRuntimePokeable] = elem match {
    case _: Bits | _: EnumType => Some(elem.asInstanceOf[Element with IsRuntimePokeable])
    case _ => None
  }
}

abstract class PeekPokeTester[T <: Module](val dut: T) {
  implicit def longToInt(x: Long) = x.toInt

  private val ctx = Driver.ctx.getOrElse(
    throw new RuntimeException("PeekPokeTester needs to be instantiated by the Driver!")
  )

  implicit val _verbose = ctx.isVerbose
  implicit val _base = ctx.base

  def println(msg: String = ""): Unit = {
    //logger.info(msg)
  }

  /** *************************
    */
  /** * Simulation Interface **
    */
  /** *************************
    */
  val backend: SimulatorContext = ctx.backend

  /** *****************************
    */
  /** * Classic Tester Interface **
    */
  /** *****************************
    */
  /* Simulation Time */
  private var simTime = 0L
  protected[iotesters] def incTime(n: Int): Unit = { simTime += n }
  def t = simTime

  /** Indicate a failure has occurred. */
  private var failureTime = -1L
  private var ok = true
  def fail(): Unit = if (ok) {
    failureTime = simTime
    ok = false
  }

  val rnd = new Random()
  ///val rnd = backend.rnd
  //rnd.setSeed(optionsManager.testerOptions.testerSeed)
  //println(s"SEED ${optionsManager.testerOptions.testerSeed}")

  /** Convert a Boolean to BigInt */
  implicit def int(x: Boolean): BigInt = if (x) 1 else 0

  /** Convert Pokeables to BigInt */
  implicit def int[T <: Element: Pokeable](x: T): BigInt = x.litValue()

  /** Convert an Int to unsigned (effectively 32-bit) BigInt
    * @param x  number to be converted
    * @return
    */
  def intToUnsignedBigInt(x: Int): BigInt = (BigInt(x >>> 1) << 1) | BigInt(x & 1)

  /** Convert an Int to unsigned (effectively 64-bit) BigInt
    * @param x long to be converted
    * @return
    */
  def longToUnsignedBigInt(x: Long): BigInt = (BigInt(x >>> 1) << 1) | BigInt(x & 1)

  def reset(n: Int = 1): Unit = {
    backend.poke("reset", 1)
    backend.step(List("clock"), n)
    backend.poke("reset", 0)
  }

  def step(n: Int): Unit = {
    if (_verbose) println(s"STEP $simTime -> ${simTime + n}")
    backend.step(List("clock"), n)
    incTime(n)
  }

  def poke(path: String, value: BigInt): Unit = backend.poke(path, value)
  def poke(path: String, value: Int):    Unit = backend.poke(path, value)
  def poke(path: String, value: Long):   Unit = backend.poke(path, value)

  def peek(path: String) = backend.peek(path)

  private def fullSignalName(e: InstanceId): String = {
    // convert instance name to (presumed..) low firrtl path
    // TODO: find a better way to do this...
    e.instanceName.split('.').mkString("_")
  }

  def poke[T <: Element: Pokeable](signal: T, value: BigInt): Unit = {
    if (!signal.isLit()) {
      backend.poke(fullSignalName(signal), maskedBigInt(value, signal.widthOption.getOrElse(256)))
    }
    // TODO: Warn if signal.isLit
  }

  def poke[T <: Element: Pokeable](signal: T, value: Int): Unit = {
    poke(signal, value.toLong)
  }

  def poke[T <: Element: Pokeable](signal: T, value: Long): Unit = {
    if (!signal.isLit()) {
      backend.poke(fullSignalName(signal), maskedLong(value, signal.widthOption.getOrElse(256)))
    }
  }

  /*
  Some backends, verilator in particular will not check to see if too many
  bits are part of input
   */
  private def maskedBigInt(bigInt: BigInt, width: Int): BigInt = bigInt & ((BigInt(1) << width) - 1)
  private def maskedLong(value:    Long, width:   Int): Long = value & ((1L << width) - 1)

  def pokeFixedPoint(signal: FixedPoint, value: Double): Unit = {
    // TODO
    //val bigInt = value.F(signal.width, signal.binaryPoint).litValue()
    //poke(signal, bigInt)
  }

  def pokeFixedPointBig(signal: FixedPoint, value: BigDecimal): Unit = {
    // TODO
    //val bigInt = value.F(signal.width, signal.binaryPoint).litValue()
    //poke(signal, bigInt)
  }

  def pokeInterval(signal: Interval, value: Double): Unit = {
    // TODO
    //val bigInt = value.I(signal.width, signal.binaryPoint).litValue()
    //poke(signal, bigInt)
  }

  def pokeIntervalBig(signal: Interval, value: BigDecimal): Unit = {
    // TODO
    //val bigInt = value.I(signal.width, signal.binaryPoint).litValue()
    //poke(signal, bigInt)
  }

  /** Locate a specific bundle element, given a name path.
    * TODO: Handle Vecs
    *
    * @param path - js (presumably bundles) terminating in a non-bundle (e.g., Bits) element.
    * @param bundle - bundle containing the element
    * @return the element (as Element)
    */
  private def getBundleElement(path: List[String], bundle: SeqMap[String, Data]): Element = {
    (path, bundle(path.head)) match {
      case (head :: Nil, element: Element) => element
      case (head :: tail, b: Bundle) => getBundleElement(tail, b.elements)
      case _ => throw new Exception(s"peek/poke bundle element mismatch $path")
    }
  }

  /** Poke a Bundle given a map of elements and values.
    *
    * @param signal the bundle to be poked
    * @param map a map from names (using '.' to delimit bundle elements), to BigInt values
    */
  def poke(signal: Bundle, map: Map[String, BigInt]): Unit = {
    val circuitElements = signal.elements
    for ((key, value) <- map) {
      val subKeys = key.split('.').toList
      val element = getBundleElement(subKeys, circuitElements)
      element match {
        case Pokeable(e) => poke(e, value)
        case _           => throw new Exception(s"Cannot poke type ${element.getClass.getName}")
      }
    }
  }

  /** Old "flatten" functionality.
    *
    * @param signal - Chisel type for which individual elements are required.
    * @return [[IndexedSeq[Element]]]
    */
  private def extractElementBits(signal: Data): IndexedSeq[Element] = {
    signal match {
      case elt: Aggregate => elt.getElements.toIndexedSeq.flatMap(extractElementBits)
      case elt: Element   => IndexedSeq(elt)
      case elt => throw new Exception(s"Cannot extractElementBits for type ${elt.getClass.getName}")
    }
  }

  def poke(signal: Aggregate, value: IndexedSeq[BigInt]): Unit = {
    (extractElementBits(signal).zip(value.reverse)).foreach { case (elem, value) =>
      elem match {
        case Pokeable(e) => poke(e, value)
        case _           => throw new Exception(s"Cannot poke type ${elem.getClass.getName}")
      }
    }
  }

  def pokeAt[TT <: Element: Pokeable](data: MemBase[TT], value: BigInt, off: Int): Unit = {
    backend.pokeMemory(fullSignalName(data), off, value)
  }

  def peek[T <: Element: Pokeable](signal: T): BigInt = {
    if (!signal.isLit()) backend.peek(fullSignalName(signal)) else signal.litValue()
  }

  /** Returns the value signal as a Double. Double may not be big enough to contain the
    * value without precision loss. This situation will Throw ChiselException
    * Consider using the more reliable [[peekFixedPointBig]]
    *
    * @param signal
    * @return
    */
  def peekFixedPoint(signal: FixedPoint): Double = {
    val bigInt = peek(signal)
    signal.binaryPoint match {
      case KnownBinaryPoint(bp) => FixedPoint.toDouble(bigInt, bp)
      case _                    => throw new Exception("Cannot peekFixedPoint with unknown binary point location")
    }
  }

  /** returns the value of signal as BigDecimal
    *
    * @param signal
    * @return
    */
  def peekFixedPointBig(signal: FixedPoint): BigDecimal = {
    val bigInt = peek(signal)
    signal.binaryPoint match {
      case KnownBinaryPoint(bp) => FixedPoint.toBigDecimal(bigInt, bp)
      case _                    => throw new Exception("Cannot peekFixedPoint with unknown binary point location")
    }
  }

  /** Returns the value signal as a Double. Double may not be big enough to contain the
    * value without precision loss. This situation will Throw ChiselException
    * Consider using the more reliable [[peekIntervalBig]]
    *
    * @param signal
    * @return
    */
  def peekInterval(signal: Interval): Double = {
    val bigInt = peek(signal)
    signal.binaryPoint match {
      case KnownBinaryPoint(bp) => Interval.toDouble(bigInt, bp)
      case _                    => throw new Exception("Cannot peekInterval with unknown binary point location")
    }
  }

  /** returns the value of signal as BigDecimal
    *
    * @param signal
    * @return
    */
  def peekIntervalBig(signal: Interval): BigDecimal = {
    val bigInt = peek(signal)
    signal.binaryPoint match {
      case KnownBinaryPoint(bp) => Interval.toBigDecimal(bigInt, bp)
      case _                    => throw new Exception("Cannot peekInterval with unknown binary point location")
    }
  }

  def peek(signal: Aggregate): Seq[BigInt] = {
    extractElementBits(signal).map(x => backend.peek(fullSignalName(x)))
  }

  /** Populate a map of names ("dotted Bundles) to Elements.
    * TODO: Deal with Vecs
    *
    * @param map the map to be constructed
    * @param indexPrefix an array of Bundle name prefixes
    * @param signalName the signal to be added to the map
    * @param signalData the signal object to be added to the map
    */
  private def setBundleElement(
    map:         mutable.LinkedHashMap[String, Element],
    indexPrefix: ArrayBuffer[String],
    signalName:  String,
    signalData:  Data
  ): Unit = {
    indexPrefix += signalName
    signalData match {
      case bundle: Bundle =>
        for ((name, value) <- bundle.elements) {
          setBundleElement(map, indexPrefix, name, value)
        }
      case elem: Element =>
        val index = indexPrefix.mkString(".")
        map(index) = elem
    }
    indexPrefix.remove(indexPrefix.size - 1)
  }

  /** Peek an aggregate (Bundle) signal.
    *
    * @param signal the signal to peek
    * @return a map of signal names ("dotted" Bundle) to BigInt values.
    */
  def peek(signal: Bundle): mutable.LinkedHashMap[String, BigInt] = {
    val elemMap = mutable.LinkedHashMap[String, Element]()
    val index = ArrayBuffer[String]()
    // Populate the Element map.
    for ((elementName, elementValue) <- signal.elements) {
      setBundleElement(elemMap, index, elementName, elementValue)
    }
    val bigIntMap = mutable.LinkedHashMap[String, BigInt]()
    elemMap.foreach {
      case (name, Pokeable(e)) => bigIntMap(name) = peek(e)
      case default             => throw new Exception(s"Cannot peek type ${default.getClass.getName}")
    }

    bigIntMap
  }

  def peekAt[TT <: Element: Pokeable](data: MemBase[TT], off: Int): BigInt = {
    backend.peekMemory(fullSignalName(data), off)
  }

  def expect(good: Boolean, msg: => String): Boolean = {
    if (_verbose || !good) println(s"""EXPECT AT $simTime $msg ${if (good) "PASS" else "FAIL"}""")
    if (!good) fail()
    good
  }

  def expect[T <: Element: Pokeable](signal: T, expected: BigInt, msg: => String = ""): Boolean = {
    if (!signal.isLit()) {
      val good = expect(peek(signal) == expected, msg)
      if (!good) fail()
      good
    } else expect(signal.litValue() == expected, s"${signal.litValue()} == $expected")
  }

  def expect[T <: Element: Pokeable](signal: T, expected: Int, msg: => String): Boolean = {
    expect(signal, BigInt(expected), msg)
  }

  /** Uses a Double as the expected value
    *
    * Consider using the more reliable [[expectFixedPointBig]]
    *
    * @param signal    signal
    * @param expected  value expected
    * @param msg       message on failure
    * @param epsilon   error bounds on expected value are +/- epsilon
    * @return
    */
  def expectFixedPoint(signal: FixedPoint, expected: Double, msg: => String, epsilon: Double = 0.01): Boolean = {
    val double = peekFixedPoint(signal)

    expect((double - expected).abs < epsilon, msg)
  }

  /** Uses a BigDecimal as the expected value
    *
    * @param signal    signal
    * @param expected  value expected
    * @param msg       message on failure
    * @param epsilon   error bounds on expected value are +/- epsilon
    * @return
    */
  def expectFixedPointBig(
    signal:   FixedPoint,
    expected: BigDecimal,
    msg:      => String,
    epsilon:  BigDecimal = 0.01
  ): Boolean = {
    val bigDecimal = peekFixedPointBig(signal)

    expect((bigDecimal - expected).abs < epsilon, msg)
  }

  /** Uses a Double as the expected value
    *
    * Consider using the more reliable [[expectIntervalBig]]
    *
    * @param signal    signal
    * @param expected  value expected
    * @param msg       message on failure
    * @param epsilon   error bounds on expected value are +/- epsilon
    * @return
    */
  def expectInterval(signal: Interval, expected: Double, msg: => String, epsilon: Double = 0.01): Boolean = {
    val double = peekInterval(signal)

    expect((double - expected).abs < epsilon, msg)
  }

  /** Uses a BigDecimal as the expected value
    *
    * @param signal    signal
    * @param expected  value expected
    * @param msg       message on failure
    * @param epsilon   error bounds on expected value are +/- epsilon
    * @return
    */
  def expectIntervalBig(signal: Interval, expected: BigDecimal, msg: => String, epsilon: BigDecimal = 0.01): Boolean = {
    val bigDecimal = peekIntervalBig(signal)

    expect((bigDecimal - expected).abs < epsilon, msg)
  }

  def expect(signal: Aggregate, expected: IndexedSeq[BigInt]): Boolean = {
    extractElementBits(signal).zip(expected.reverse).forall { case (elem, value) =>
      elem match {
        case Pokeable(e) => expect(e, value)
        case default     => throw new Exception(s"Cannot expect type ${default.getClass.getName}")
      }
    }
  }

  /** Return true or false if an aggregate signal (Bundle) matches the expected map of values.
    * TODO: deal with Vecs
    *
    * @param signal the Bundle to "expect"
    * @param expected a map of signal names ("dotted" Bundle notation) to BigInt values
    * @return true if the specified values match, false otherwise.
    */
  def expect(signal: Bundle, expected: Map[String, BigInt]): Boolean = {
    val elemMap = mutable.LinkedHashMap[String, Element]()
    val index = ArrayBuffer[String]()
    for ((elementName, elementValue) <- signal.elements) {
      setBundleElement(elemMap, index, elementName, elementValue)
    }
    expected.forall { case (name, value) =>
      elemMap(name) match {
        case Pokeable(e) => expect(e, value)
        case default     => throw new Exception(s"Cannot expect type ${default.getClass.getName}")
      }
    }
  }

  def finish: Boolean = {
    backend.finish()
    println(s"""RAN $simTime CYCLES ${if (ok) "PASSED" else s"FAILED FIRST AT CYCLE $failureTime"}""")
    //logger.report()
    ok
  }
}
