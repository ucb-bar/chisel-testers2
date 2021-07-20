// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator.ipc

import chiseltest.simulator._
import firrtl.AnnotationSeq
import logger.LazyLogging

import java.io.File
import java.nio.channels.FileChannel
import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._
import scala.concurrent.{blocking, Await, ExecutionContext, Future}
import scala.sys.process._

/** This context works with a simulation binary that communicates through shared memory.
  * @param cmd command to launch the simulation binary
  * @param toplevel information about the interface exposed by the module at the top of the RTL hierarchy
  * @param waveformFile optional path to the waveform file, we assume that the file will only be available _after_
  *                     the simulation has been terminated
  * @param loadCoverage may be called _after_ the simulation is finished to retrieve coverage information
  * @param sim simulator that generated the binary
  */
private[chiseltest] class IPCSimulatorContext(
  bin:              os.Path,
  toplevel:         TopmoduleInfo,
  coverageAnnos:    AnnotationSeq,
  override val sim: Simulator)
    extends SimulatorContext
    with LazyLogging {
  require(toplevel.clocks.length <= 1, "Multi clock circuits are currently not supported!")

  // Construct maps for the input and output
  private val (inputsNameToChunkSizeMap, outputsNameToChunkSizeMap) = {
    def genChunk(port: (String, Int)): (String, Int) = port._1 -> ((port._2 - 1) / 64 + 1)
    (
      ListMap(toplevel.inputs.map(genChunk):  _*),
      ListMap(toplevel.outputs.map(genChunk): _*)
    )
  }
  private object SIM_CMD extends Enumeration {
    val RESET, STEP, UPDATE, POKE, PEEK, FORCE, GETID, GETCHK, FIN = Value
  }
  def int(x: Int):  BigInt = (BigInt(x >>> 1) << 1) | BigInt(x & 1)
  def int(x: Long): BigInt = (BigInt(x >>> 1) << 1) | BigInt(x & 1)

  private var isStale = false
  private val _pokeMap = mutable.HashMap[String, BigInt]()
  private val _peekMap = mutable.HashMap[String, BigInt]()
  private val _signalMap = mutable.HashMap[String, Int]()
  private val _chunks = mutable.HashMap[String, Int]()
  private val _logs = mutable.ArrayBuffer[String]()

  private def startProcess(cmd: Seq[String], logs: ArrayBuffer[String]): Process = {
    require(new java.io.File(cmd.head).exists, s"${cmd.head} doesn't exist")
    val processBuilder = Process(cmd.mkString(" "))
    // This makes everything written to stderr get added as lines to logs
    val processLogger = ProcessLogger(println, logs += _) // don't log stdout
    processBuilder.run(processLogger)
  }

  //initialize simulator process
  private[chiseltest] val process = startProcess(Seq(bin.toString()), _logs)

  // Set up a Future to wait for (and signal) the test process exit.
  import ExecutionContext.Implicits.global
  private[chiseltest] val exitValue = Future(blocking(process.exitValue()))

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

  private def dumpLogs(): Unit = {
    _logs.foreach(x => println(x))
    _logs.clear()
  }

  private def throwExceptionIfDead(exitValue: Future[Int]): Unit = {
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
      isRunning = false
      throw TestApplicationException(exitCode, errorString)
    }
  }

  /** A busy-wait loop that monitors exitValue so we don't loop forever if the test application exits for some reason.
    *
    * @param block  a thunk that determines when complete
    * @param loop   a thunk to keep running until block is true or exitValue says completed.
    */
  private def mwhile(block: => Boolean)(loop: => Unit): Unit = {
    while (!exitValue.isCompleted && block) {
      loop
    }
    // If the test application died, throw a run-time error.
    throwExceptionIfDead(exitValue)
  }

  private def sendCmd(cmd: SIM_CMD.Value): Boolean = sendCmd(cmd.id)

  private def sendCmd(data: Int): Boolean = {
    cmdChannel.acquire()
    val ready = cmdChannel.ready
    if (ready) {
      cmdChannel(0) = data
      cmdChannel.produce()
    }
    cmdChannel.release()
    ready
  }

  private def sendCmd(data: String): Boolean = {
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
          (0 until chunk).foldLeft(BigInt(0))((res, i) => res | (int(outChannel(i)) << (64 * i)))
        )
      }
    outChannel.release()
    value
  }

  private def recvOutputs = {
    _peekMap.clear()
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

  private def poke(id: Int, chunk: Int, v: BigInt, force: Boolean = false): Unit = {
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
    println(s"""STARTING $bin""")
    mwhile(!recvOutputs) {}
    isRunning = true
  }

  override def poke(signal: String, value: BigInt): Unit = {
    if (inputsNameToChunkSizeMap.contains(signal)) {
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

  override def peek(signal: String): BigInt = {
    if (isStale) update()
    val r = if (outputsNameToChunkSizeMap.contains(signal)) {
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
    r.getOrElse(throw new RuntimeException(s"Could not find signal $signal"))
  }

  private def defaultClock = toplevel.clocks.headOption
  override def step(n: Int): Unit = {
    defaultClock match {
      case Some(value) =>
      case None        => throw new NoClockException(toplevel.name)
    }
    try {
      update()
      (0 until n).foreach(_ => takeStep())
    } catch {
      case TestApplicationException(exit, msg) =>
      // TODO: throw exception
    }
  }

  private var isRunning = false
  override def finish(): Unit = {
    mwhile(!sendCmd(SIM_CMD.FIN)) {}
    val exit = Await.result(exitValue, Duration.Inf)
    println("Exit Code: %d".format(exit))
    dumpLogs()
    inChannel.close()
    outChannel.close()
    cmdChannel.close()
    isRunning = false
  }

  // Once everything has been prepared, we can start the communications.
  start()

  private val coverageFile = bin / os.up / os.up / "logs" / "coverage.dat"
  override def getCoverage(): List[(String, Long)] = {
    if (isRunning) {
      throw new RuntimeException(
        "This backend does not support providing coverage while the simulation is still running."
      )
    } else {
      assert(os.exists(coverageFile), s"Could not find `$coverageFile` file!")
      VerilatorCoverage.loadCoverage(coverageAnnos, coverageFile)
    }
  }

  override def resetCoverage(): Unit = {
    throw new NotImplementedError("getCoverage")
  }
}

private[chiseltest] object TesterProcess {
  def apply(cmd: Seq[String], logs: ArrayBuffer[String]): Process = {
    require(new java.io.File(cmd.head).exists, s"${cmd.head} doesn't exist")
    val processBuilder = Process(cmd.mkString(" "))
    // This makes everything written to stderr get added as lines to logs
    val processLogger = ProcessLogger(println, logs += _) // don't log stdout
    processBuilder.run(processLogger)
  }
}

private[chiseltest] class Channel(name: String) {
  private lazy val file = new java.io.RandomAccessFile(name, "rw")
  private lazy val channel = file.getChannel
  @volatile private lazy val buffer = {
    val size = channel.size
    assert(size > 16, "channel.size is bogus: %d".format(size))
    channel.map(FileChannel.MapMode.READ_WRITE, 0, size)
  }

  val channel_data_offset_64bw = 4 // Offset from start of channel buffer to actual user data in 64bit words.
  def acquire(): Unit = {
    buffer.put(0, 1)
    buffer.put(2, 0)
    while (buffer.get(1) == 1 && buffer.get(2) == 0) {}
  }
  def release(): Unit = { buffer.put(0, 0) }
  def ready: Boolean = buffer.get(3) == 0
  def valid: Boolean = buffer.get(3) == 1
  def produce(): Unit = { buffer.put(3, 1) }
  def consume(): Unit = { buffer.put(3, 0) }
  def update(idx: Int, data: Long): Unit = {
    buffer.putLong(8 * idx + channel_data_offset_64bw, data)
  }
  def update(base: Int, data: String): Unit = {
    data.zipWithIndex.foreach { case (c, i) =>
      buffer.put(base + i + channel_data_offset_64bw, c.toByte)
    }
    buffer.put(base + data.length + channel_data_offset_64bw, 0)
  }
  def apply(idx: Int): Long = buffer.getLong(8 * idx + channel_data_offset_64bw)
  def close(): Unit = { file.close() }
  buffer.order(java.nio.ByteOrder.nativeOrder)
  new File(name).delete
}

private[chiseltest] case class TestApplicationException(exitVal: Int, lastMessage: String)
    extends RuntimeException(lastMessage)
