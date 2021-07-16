// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator.ipc

import chiseltest.simulator.TopmoduleInfo

/** Generates the Module specific Verilog harness file for a VPI based interface */
private[chiseltest] object VpiVerilogHarnessGenerator {
  def codeGen(
    toplevel:    TopmoduleInfo,
    vcdFilePath: os.Path,
    isGateLevel: Boolean = false
  ): String = {
    val dutName = toplevel.name

    val codeBuffer = new StringBuilder
    codeBuffer.append("module test;\n")
    codeBuffer.append("  reg clock = 1;\n")
    codeBuffer.append("  reg reset = 1;\n")
    val delay = if (isGateLevel) "#0.1" else ""
    toplevel.inputs.foreach { case (name, width) =>
      codeBuffer.append(s"  reg[${width - 1}:0] $name = 0;\n")
      codeBuffer.append(s"  wire[${width - 1}:0] ${name}_delay;\n")
      codeBuffer.append(s"  assign $delay ${name}_delay = $name;\n")
    }
    toplevel.outputs.foreach { case (name, width) =>
      codeBuffer.append(s"  wire[${width - 1}:0] ${name}_delay;\n")
      codeBuffer.append(s"  wire[${width - 1}:0] $name;\n")
      codeBuffer.append(s"  assign $delay $name = ${name}_delay;\n")
    }

    codeBuffer.append("  always #`CLOCK_PERIOD clock = ~clock;\n")
    codeBuffer.append("  reg vcdon = 0;\n")
    codeBuffer.append("  reg [1023:0] vcdfile = 0;\n")
    codeBuffer.append("  reg [1023:0] vpdfile = 0;\n")

    codeBuffer.append("\n  /*** DUT instantiation ***/\n")
    codeBuffer.append(s"  $dutName $dutName(\n")
    codeBuffer.append("    .clock(clock),\n")
    codeBuffer.append("    .reset(reset),\n")
    val ioNames = (toplevel.inputs ++ toplevel.outputs).map(_._1)
    codeBuffer.append(ioNames.map(name => s"    .$name(${name}_delay)").mkString(",\n"))
    codeBuffer.append("  );\n\n")

    codeBuffer.append("  initial begin\n")
    codeBuffer.append("    $init_rsts(reset);\n")
    val inputNames = toplevel.inputs.map(_._1)
    codeBuffer.append("    $init_ins(%s);\n".format(inputNames.mkString(", ")))
    val outputNames = toplevel.outputs.map(_._1)
    codeBuffer.append("    $init_outs(%s);\n".format(outputNames.mkString(", ")))
    codeBuffer.append("    $init_sigs(%s);\n".format(dutName))
    codeBuffer.append("    /*** VCD & VPD dump ***/\n")
    codeBuffer.append("    if ($value$plusargs(\"vcdfile=%s\", vcdfile)) begin\n")
    codeBuffer.append("      $dumpfile(vcdfile);\n")
    codeBuffer.append("      $dumpvars(0, %s);\n".format(dutName))
    codeBuffer.append("      $dumpoff;\n")
    codeBuffer.append("      vcdon = 0;\n")
    codeBuffer.append("    end\n")
    codeBuffer.append("    if ($value$plusargs(\"waveform=%s\", vpdfile)) begin\n")
    codeBuffer.append("      $vcdplusfile(vpdfile);\n")
    codeBuffer.append("    end else begin\n")
    codeBuffer.append("      $vcdplusfile(\"%s\");\n".format(vcdFilePath))
    codeBuffer.append("    end\n")
    codeBuffer.append("    if ($test$plusargs(\"vpdmem\")) begin\n")
    codeBuffer.append("      $vcdplusmemon;\n")
    codeBuffer.append("    end\n")
    codeBuffer.append("    $vcdpluson(0);\n")
    codeBuffer.append("  end\n\n")

    codeBuffer.append("  always @(%s clock) begin\n".format(if (isGateLevel) "posedge" else "negedge"))
    codeBuffer.append("    if (vcdfile && reset) begin\n")
    codeBuffer.append("      $dumpoff;\n")
    codeBuffer.append("      vcdon = 0;\n")
    codeBuffer.append("    end\n")
    codeBuffer.append("    else if (vcdfile && !vcdon) begin\n")
    codeBuffer.append("      $dumpon;\n")
    codeBuffer.append("      vcdon = 1;\n")
    codeBuffer.append("    end\n")
    codeBuffer.append("    %s $tick();\n".format(if (isGateLevel) "#0.05" else ""))
    codeBuffer.append("    $vcdplusflush;\n")
    codeBuffer.append("  end\n\n")
    codeBuffer.append("endmodule\n")

    codeBuffer.toString()
  }
}
