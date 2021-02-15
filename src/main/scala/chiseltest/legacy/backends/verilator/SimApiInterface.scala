// SPDX-License-Identifier: Apache-2.0

package chiseltest.legacy.backends.verilator

import java.io.File
import java.nio.channels.FileChannel

import chisel3._
import logger.LazyLogging

import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{blocking, Await, ExecutionContext, Future}
import scala.language.implicitConversions

/** Provides an interface layer to an external verilog simulation program through IPC channels
  * Currently supported external programs are those generated by verilator and VCS.
  *
  * @param dut  The device under test
  * @param cmd  The command to run as a Seq of strings
  */
private[chiseltest] class SimApiInterface(dut: Module, cmd: Seq[String]) extends LazyLogging {
  //
  // Construct maps for the input and output
  // Remove zero length fields during the process
  //
  val (inputsNameToChunkSizeMap, outputsNameToChunkSizeMap) = {
    val (inputs, outputs) = getPorts(dut)

    def genChunk(args: (Data, String)): Option[(String, Int)] = args match {
      case (pin, name) if pin.getWidth > 0 =>
        //TODO: This hack needs to be replaced by something rational
        val n = if (name.endsWith("reset")) {
          "reset"
        } else if (name.endsWith("clock")) {
          "clock"
        } else {
          name
        }

        Some(n -> ((pin.getWidth - 1) / 64 + 1))
      case _ => None
    }

    (
      ListMap(inputs.flatMap(genChunk):  _*),
      ListMap(outputs.flatMap(genChunk): _*)
    )
  }
  private object SIM_CMD extends Enumeration {
    val RESET, STEP, UPDATE, POKE, PEEK, FORCE, GETID, GETCHK, FIN = Value
  }
  implicit def cmdToId(cmd: SIM_CMD.Value): Int = cmd.id
  implicit def int(x:       Int):           BigInt = (BigInt(x >>> 1) << 1) | BigInt(x & 1)
  implicit def int(x:       Long):          BigInt = (BigInt(x >>> 1) << 1) | BigInt(x & 1)

  private var isStale = false
  private val _pokeMap = mutable.HashMap[String, BigInt]()
  private val _peekMap = mutable.HashMap[String, BigInt]()
  private val _signalMap = mutable.HashMap[String, Int]()
  private val _chunks = mutable.HashMap[String, Int]()
  private val _logs = mutable.ArrayBuffer[String]()

  //initialize simulator process
  private[chiseltest] val process = TesterProcess(cmd, _logs)

  // Set up a Future to wait for (and signal) the test process exit.
  import ExecutionContext.Implicits.global
  private[chiseltest] val exitValue = Future(blocking(process.exitValue))

  // memory mapped channels
  private val (inChannel, outChannel, cmdChannel) = {
    // Wait for the startup message
    // NOTE: There may be several messages before we see our startup message.
    val simStartupMessageStart = "sim start on "
    while (!_logs.exists(_.startsWith(simStartupMessageStart)) && !exitValue.isCompleted) {
      Thread.sleep(100)
    }
    // Remove the startup message (and any precursors).
    while (_logs.nonEmpty && !_logs.head.startsWith(simStartupMessageStart)) {
      println(_logs.remove(0))
    }
    println(if (_logs.nonEmpty) _logs.remove(0) else "<no startup message>")
    while (_logs.size < 3) {
      // If the test application died, throw a run-time error.
      throwExceptionIfDead(exitValue)
      Thread.sleep(100)
    }
    val in_channel_name = _logs.remove(0)
    val out_channel_name = _logs.remove(0)
    val cmd_channel_name = _logs.remove(0)
    val in_channel = new Channel(in_channel_name)
    val out_channel = new Channel(out_channel_name)
    val cmd_channel = new Channel(cmd_channel_name)

    println(s"inChannelName: $in_channel_name")
    println(s"outChannelName: $out_channel_name")
    println(s"cmdChannelName: $cmd_channel_name")

    in_channel.consume()
    cmd_channel.consume()
    in_channel.release()
    out_channel.release()
    cmd_channel.release()

    (in_channel, out_channel, cmd_channel)
  }

  private def dumpLogs() {
    _logs.foreach(x => println(x))
    _logs.clear
  }

  private def throwExceptionIfDead(exitValue: Future[Int]) {
//    implicit val logger = new TestErrorLog
    if (exitValue.isCompleted) {
      val exitCode = Await.result(exitValue, Duration(-1, SECONDS))
      // We assume the error string is the last log entry.
      val errorString = if (_logs.nonEmpty) {
        _logs.last
      } else {
        "test application exit"
      } + " - exit code %d".format(exitCode)
      dumpLogs()
      throw TestApplicationException(exitCode, errorString)
    }
  }

  /** A busy-wait loop that monitors exitValue so we don't loop forever if the test application exits for some reason.
    *
    * @param block  a thunk that determines when complete
    * @param loop   a thunk to keep running until block is true or exitValue says completed.
    */
  private def mwhile(block: => Boolean)(loop: => Unit) {
    while (!exitValue.isCompleted && block) {
      loop
    }
    // If the test application died, throw a run-time error.
    throwExceptionIfDead(exitValue)
  }

  private def sendCmd(data: Int) = {
    cmdChannel.acquire()
    val ready = cmdChannel.ready
    if (ready) {
      cmdChannel(0) = data
      cmdChannel.produce()
    }
    cmdChannel.release()
    ready
  }

  private def sendCmd(data: String) = {
    cmdChannel.acquire()
    val ready = cmdChannel.ready
    if (ready) {
      cmdChannel(0) = data
      cmdChannel.produce()
    }
    cmdChannel.release()
    ready
  }

  private def recvResp = {
    outChannel.acquire()
    val valid = outChannel.valid
    val resp =
      if (!valid) None
      else {
        outChannel.consume()
        Some(outChannel(0).toInt)
      }
    outChannel.release()
    resp
  }

  private def sendValue(value: BigInt, chunk: Int) = {
    inChannel.acquire()
    val ready = inChannel.ready
    if (ready) {
      (0 until chunk).foreach(i => inChannel(i) = (value >> (64 * i)).toLong)
      inChannel.produce()
    }
    inChannel.release()
    ready
  }

  private def recvValue(chunk: Int) = {
    outChannel.acquire()
    val valid = outChannel.valid
    val value =
      if (!valid) None
      else {
        outChannel.consume()
        Some(
          ((0 until chunk).foldLeft(BigInt(0)))((res, i) => res | (int(outChannel(i)) << (64 * i)))
        )
      }
    outChannel.release()
    value
  }

  private def recvOutputs = {
    _peekMap.clear
    outChannel.acquire()
    val valid = outChannel.valid
    if (valid) {
      (outputsNameToChunkSizeMap.toList.foldLeft(0)) { case (off, (out, chunk)) =>
        _peekMap(out) = ((0 until chunk).foldLeft(BigInt(0)))((res, i) => res | (int(outChannel(off + i)) << (64 * i)))
        off + chunk
      }
      outChannel.consume()
    }
    outChannel.release()
    valid
  }

  private def sendInputs = {
    inChannel.acquire()
    val ready = inChannel.ready
    if (ready) {
      (inputsNameToChunkSizeMap.toList.foldLeft(0)) { case (off, (in, chunk)) =>
        val value = _pokeMap.getOrElse(in, BigInt(0))
        (0 until chunk).foreach(i => inChannel(off + i) = (value >> (64 * i)).toLong)
        off + chunk
      }
      inChannel.produce()
    }
    inChannel.release()
    ready
  }

  private def update(): Unit = {
    mwhile(!sendCmd(SIM_CMD.UPDATE)) {}
    mwhile(!sendInputs) {}
    mwhile(!recvOutputs) {}
    isStale = false
  }

  private def takeStep(): Unit = {
    mwhile(!sendCmd(SIM_CMD.STEP)) {}
    mwhile(!sendInputs) {}
    mwhile(!recvOutputs) {}
    dumpLogs()
  }

  private def getId(path: String) = {
    mwhile(!sendCmd(SIM_CMD.GETID)) {}
    mwhile(!sendCmd(path)) {}
    if (exitValue.isCompleted) {
      0
    } else {
      (for {
        _ <- Stream.from(1)
        data = recvResp
        if data.isDefined
      } yield data.get).head
    }
  }

  private def getChunk(id: Int) = {
    mwhile(!sendCmd(SIM_CMD.GETCHK)) {}
    mwhile(!sendCmd(id)) {}
    if (exitValue.isCompleted) {
      0
    } else {
      (for {
        _ <- Stream.from(1)
        data = recvResp
        if data.isDefined
      } yield data.get).head
    }
  }

  private def poke(id: Int, chunk: Int, v: BigInt, force: Boolean = false) {
    val cmd = if (!force) SIM_CMD.POKE else SIM_CMD.FORCE
    mwhile(!sendCmd(cmd)) {}
    mwhile(!sendCmd(id)) {}
    mwhile(!sendValue(v, chunk)) {}
  }

  private def peek(id: Int, chunk: Int): BigInt = {
    mwhile(!sendCmd(SIM_CMD.PEEK)) {}
    mwhile(!sendCmd(id)) {}
    if (exitValue.isCompleted) {
      BigInt(0)
    } else {
      (for {
        _ <- Stream.from(1)
        data = recvValue(chunk)
        if data.isDefined
      } yield data.get).head
    }
  }

  private def start(): Unit = {
    println(s"""STARTING ${cmd.mkString(" ")}""")
    mwhile(!recvOutputs) {}

    //TODO: Figure out what the behavior of the interface on start up is
//    for (_ <- 0 until 5) {
//      mwhile(!sendCmd(SIM_CMD.RESET)) { }
//      mwhile(!recvOutputs) { }
//    }
  }

  def poke(signal: String, value: BigInt) {
    if (inputsNameToChunkSizeMap contains signal) {
      _pokeMap(signal) = value
      isStale = true
    } else {
      val id = _signalMap.getOrElseUpdate(signal, getId(signal))
      if (id >= 0) {
        poke(id, _chunks.getOrElseUpdate(signal, getChunk(id)), value)
        isStale = true
      } else {
        logger.info(s"Can't find $signal in the emulator...")
      }
    }
  }

  def peek(signal: String): Option[BigInt] = {
    if (isStale) update()
    if (outputsNameToChunkSizeMap.contains(signal)) {
      _peekMap.get(signal)
    } else if (inputsNameToChunkSizeMap.contains(signal)) {
      // Added this in case peek is called on input before poke. Did not seem to be a problem in testers
      if (!_pokeMap.contains(signal)) {
        _pokeMap(signal) = BigInt(0)
        isStale = true
        update()
      }
      _pokeMap.get(signal)
    } else {
      val id = _signalMap.getOrElseUpdate(signal, getId(signal))
      if (id >= 0) {
        Some(peek(id, _chunks.getOrElse(signal, getChunk(id))))
      } else {
        logger.info(s"Can't find $signal in the emulator...")
        None
      }
    }
  }

  def step(n: Int) {
    update()
    (0 until n).foreach(_ => takeStep())
  }

  def reset(n: Int = 1) {
    for (_ <- 0 until n) {
      mwhile(!sendCmd(SIM_CMD.RESET)) {}
      mwhile(!recvOutputs) {}
    }
  }

  def finish(): Unit = {
    mwhile(!sendCmd(SIM_CMD.FIN)) {}
    println("Exit Code: %d".format(Await.result(exitValue, Duration.Inf)))
    dumpLogs()
    inChannel.close()
    outChannel.close()
    cmdChannel.close()
  }

  // Once everything has been prepared, we can start the communications.
  start()
}

private[chiseltest] class Channel(name: String) {
  private lazy val file = new java.io.RandomAccessFile(name, "rw")
  private lazy val channel = file.getChannel
  @volatile private lazy val buffer = {
    val size = channel.size
    assert(size > 16, "channel.size is bogus: %d".format(size))
    channel.map(FileChannel.MapMode.READ_WRITE, 0, size)
  }
  implicit def intToByte(i: Int): Byte = i.toByte
  val channel_data_offset_64bw = 4 // Offset from start of channel buffer to actual user data in 64bit words.
  def acquire(): Unit = {
    buffer.put(0, 1)
    buffer.put(2, 0)
    while ((buffer.get(1)) == 1 && (buffer.get(2)) == 0) {}
  }
  def release(): Unit = { buffer.put(0, 0) }
  def ready: Boolean = (buffer.get(3)) == 0
  def valid: Boolean = (buffer.get(3)) == 1
  def produce(): Unit = { buffer.put(3, 1) }
  def consume(): Unit = { buffer.put(3, 0) }
  def update(idx: Int, data: Long) {
    buffer.putLong(8 * idx + channel_data_offset_64bw, data)
  }
  def update(base: Int, data: String) {
    data.zipWithIndex.foreach { case (c, i) =>
      buffer.put(base + i + channel_data_offset_64bw, c)
    }
    buffer.put(base + data.length + channel_data_offset_64bw, 0)
  }
  def apply(idx: Int): Long =
    buffer.getLong(8 * idx + channel_data_offset_64bw)
  def close(): Unit = { file.close() }
  buffer.order(java.nio.ByteOrder.nativeOrder)
  new File(name).delete
}
