// See LICENSE for license details.

package chisel3.tester.experimental.tbdump

import chisel3._
import chisel3.experimental.{DataMirror, Direction, FixedPoint}
import java.io._

object util {
  case class TbBase(
    val buff: BufferedWriter,
    val inputs: Seq[(String, Data)] = Nil,
    val outputs: Seq[(String, Data)] = Nil
  )

  abstract class BaseDumpTester{
    def dut : MultiIOModule
    val testersOpt: Option[TbDumpOptions] = Some(TbDumpOptions())
  }

  def isSigned(e: Data): Boolean = {
    e match {
      case _: SInt | _: FixedPoint => true
      case _: Clock => false
      case _ => false
    }
  }

  def signPrefix(e: Data): String = {
    def signed = isSigned(e)
    if (signed) " signed "
    else ""
  }

  def stepPrint(n: Int)(implicit tb: Option[TbBase]) {
    tb.get.buff write s"    #($n*`CLK_PERIOD);\n"
  }

  def timeoutPrint(n: Int)(implicit tb: Option[TbBase]) {
      tb.get.buff write "\n  initial begin\n"
      tb.get.buff write "    #`CLK_PERIOD * cycle;\n"
      tb.get.buff write "    $finish;\n"
      tb.get.buff write "  end\n\n"
  }

  def resetPrint(n: Int)(implicit tb: Option[TbBase]) {
      tb.get.buff write s"    reset = 1; #($n*`CLK_PERIOD) reset = 0;\n"
  }

  def pokePrint(signal: Element, value: BigInt)(implicit tb: Option[TbBase])  {
    if(tb.isDefined){
      val matchingInput = tb.get.inputs.find(x => x._2 == signal)
      matchingInput match {
        case Some((name, _)) => {
          val bitLength = value.bitLength.max(1)
          val id = if (value >= 0) s"$bitLength\'d" else ""
          tb.get.buff write s"    $name = $id$value;\n"
        }
        case _ => // Don't print if not input
      }
    }
  }

  def expectPrint(signal: Element, value: BigInt, message: Option[String])(implicit tb: Option[TbBase])  {
    if(tb.isDefined){
      val matchingOutput = tb.get.outputs.find(x => x._2 == signal)
      matchingOutput match {
        case Some((name, _)) => {
          val bitLength = value.bitLength.max(1)
          val id = if (value >= 0) s"$bitLength\'d" else ""
          tb.get.buff write "   `expect(\"%s\",%s,%d,cycle)\n".format(signal.name,name,value)
        }
        case _ => // Don't print if not input
      }
    }
  }

  def waitPrint(signal: Element, value: BigInt)(implicit tb: Option[TbBase]) {
    if(tb.isDefined){
      val matchingOutput = tb.get.outputs.find(x => x._2 == signal)
      matchingOutput match {
        case Some((name, _)) => {
          val bitLength = value.bitLength.max(1)
          val id = if (value >= 0) s"$bitLength\'d" else ""
          tb.get.buff write s"    wait(${name} == $value);\n"
        }
        case _ => // Don't print if not input
      }
    }
  }

  def timescopeBeginPrint(implicit tb: Option[TbBase]): Unit = {
    if(tb.isDefined){
      tb.get.buff write "\n    begin\n"
    }
  }

  def timescopeEndPrint(implicit tb: Option[TbBase]): Unit = {
    if(tb.isDefined){
      tb.get.buff write "    end\n"
    }
  }

  def finishPrint(implicit tb: Option[TbBase]): Unit = {
    if (tb.isDefined) {
      tb.get.buff write "\n    #`CLK_PERIOD $display(\"\\t **Ran through all test vectors**\"); $finish;\n"
      tb.get.buff write "\n  end\n"
      tb.get.buff write "endmodule"
      tb.get.buff.close()
    }
  }


  case class TbDumpOptions(
      dir: Option[String] = Some(s"/verilog_testbench"),
      // clk period in ps = clkMul * tbTimeUnitPs
      clkMul: Int = 1,
      // Time unit in ps
      tbTimeUnitPs: Int = 100,
      // Time precision in ps
      tbTimePrecisionPs: Int = 10,
      // Input/output delay after which to peek/poke values (some fraction of clkMul)
      inOutDelay: Double = 0.5,
      // # clk periods for initial reset
      initClkPeriods: Int = 5) {

      val clkPeriodPs = tbTimeUnitPs * clkMul
      val initTimeUnits = clkMul * initClkPeriods

      require(tbTimeUnitPs >= tbTimePrecisionPs, "Time unit should be >= precision")
      require(clkPeriodPs / 2 > tbTimePrecisionPs, "Half a clk period should be greater than time precision")
      require(clkPeriodPs % 2 == 0, "Clk period should be divisible by 2")
      require(initClkPeriods >= 1, "Reset should be applied for at least 1 clk period")
  }

  trait TbDump { this: BaseDumpTester =>

    implicit val tbBase = testersOpt.map { case params =>

      val targetDir = Driver.targetDir() + params.dir.getOrElse("")

      val tbFileName = s"${targetDir}/tb.v"
      val fullDir = new File(targetDir)
      if (!fullDir.exists()) {fullDir.mkdir()}

      val buff = new BufferedWriter(new FileWriter(tbFileName))
      val ports = DataMirror.fullModulePorts(dut)
      val inputs = ports.filter(x => DataMirror.directionOf(x._2) == chisel3.ActualDirection.Input).filter {
        case ("clock", _) => false
        case ("reset", _) => false
        case _ => true
      }
      val outputs = ports.filter(x => DataMirror.directionOf(x._2) == chisel3.ActualDirection.Output)

      def initVerilogTbFile() {
          val dutName = dut.name
          val resetTime = params.initTimeUnits
          // Input/output delay after which to peek/poke values
          val clkDelta = params.clkMul * params.inOutDelay

          buff write s"// Example VCS Command: $$VCS_HOME/bin/vcs -debug_pp -full64 +define+UNIT_DELAY +rad +v2k +vcs+lic+wait " +
            s"+vc+list +vcs+initreg+random +vcs+dumpvars+out.vcd tb.v ${dutName}.v ... \n"

          buff write s"`timescale ${params.tbTimeUnitPs}ps / ${params.tbTimePrecisionPs}ps\n"
          buff write s"\n`define CLK_PERIOD ${params.clkMul}\n"
          buff write s"\n`define HALF_CLK_PERIOD ${params.clkMul.toDouble / 2}\n"
          buff write s"`define RESET_TIME ${resetTime}\n"
          buff write s"`define INIT_TIME ${clkDelta + resetTime}\n"

          buff write "`define expect(nodeName, nodeVal, expVal, cycle) if (nodeVal !== expVal) begin " +
            "\\\n  $display(\"\\t ASSERTION ON %s FAILED @ CYCLE = %d, 0x%h != EXPECTED 0x%h\", " +
            "\\\n  nodeName,cycle,nodeVal,expVal); $stop; end\n\n"

          buff write "module testbench_v;\n\n"
          buff write "  integer cycle = 0;\n\n"
          buff write "  reg clock = 1;\n"
          buff write "  reg reset = 1;\n"
          inputs.map { case (name, ele) =>
            val s = signPrefix(ele)
            buff write s"  reg$s[${ele.getWidth - 1}:0] ${name} = 0;\n"
          }
          outputs.map { case (name, ele) =>
            val s = signPrefix(ele)
            buff write s"  wire$s[${ele.getWidth - 1}:0] ${name};\n"
          }
          buff write "\n  always #`HALF_CLK_PERIOD clock = ~clock;\n"
          buff write "\n  initial begin\n"
          buff write "    #`RESET_TIME\n"
          buff write "    forever #`CLK_PERIOD cycle = cycle + 1;\n"
          buff write "  end\n\n"

          buff write s"  ${dutName} ${dutName}(\n"
          buff write "    .clock(clock),\n"
          buff write "    .reset(reset),\n"
          buff write (inputs.map(x => s"    .${x._1}(${x._1})") mkString ",\n")
          if (!inputs.isEmpty) buff write  s",\n"
          buff write (outputs.map(x => s"    .${x._1}(${x._1})") mkString ",\n")
          buff write ");\n\n"

          buff write "  initial begin\n"
          buff write "    #`INIT_TIME reset = 0;\n"
      }

      initVerilogTbFile()
      TbBase(buff, inputs, outputs)
    }
  }
}
