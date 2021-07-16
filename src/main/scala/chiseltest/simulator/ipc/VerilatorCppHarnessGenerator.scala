// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator.ipc

import chiseltest.simulator.TopmoduleInfo

/** Generates the Module specific verilator harness cpp file for verilator compilation */
private[chiseltest] object VerilatorCppHarnessGenerator {
  def codeGen(
    toplevel:     TopmoduleInfo,
    vcdFilePath:  os.Path,
    targetDir:    os.Path,
    majorVersion: Int,
    minorVersion: Int
  ): String = {

    require(toplevel.clocks.length <= 1, "Multi clock circuits are currently not supported!")
    val clockName = toplevel.clocks.headOption
    val clockLow = clockName.map("dut->" + _ + " = 0;").getOrElse("")
    val clockHigh = clockName.map("dut->" + _ + " = 1;").getOrElse("")

    val codeBuffer = new StringBuilder

    def pushBack(vector: String, pathName: String, width: BigInt): Unit = {
      if (width == 0) {
        // Do nothing- 0 width wires are removed
      } else if (width <= 8) {
        codeBuffer.append(
          s"        sim_data.$vector.push_back(new VerilatorCData(&($pathName)));\n"
        )
      } else if (width <= 16) {
        codeBuffer.append(
          s"        sim_data.$vector.push_back(new VerilatorSData(&($pathName)));\n"
        )
      } else if (width <= 32) {
        codeBuffer.append(
          s"        sim_data.$vector.push_back(new VerilatorIData(&($pathName)));\n"
        )
      } else if (width <= 64) {
        codeBuffer.append(
          s"        sim_data.$vector.push_back(new VerilatorQData(&($pathName)));\n"
        )
      } else {
        val numWords = (width - 1) / 32 + 1
        codeBuffer.append(
          s"        sim_data.$vector.push_back(new VerilatorWData($pathName, $numWords));\n"
        )
      }
    }

    val dutName = toplevel.name
    val dutApiClassName = dutName + "_api_t"
    val dutVerilatorClassName = "V" + dutName

    val coverageInit =
      if (majorVersion >= 4 && minorVersion >= 202)
        """|Verilated::defaultContextp()->coveragep()->forcePerInstance(true);
           |""".stripMargin
      else ""

    val verilatorRunFlushCallback = if (majorVersion >= 4 && minorVersion >= 38) {
      "Verilated::runFlushCallbacks();\nVerilated::runExitCallbacks();\n"
    } else {
      "Verilated::flushCall();\n"
    }
    codeBuffer.append(s"""
#include "$dutVerilatorClassName.h"
#include "verilated.h"
#include "veri_api.h"
#if VM_TRACE
#include "verilated_vcd_c.h"
#endif
#include <iostream>
class $dutApiClassName: public sim_api_t<VerilatorDataWrapper*> {
    public:
    $dutApiClassName($dutVerilatorClassName* _dut) {
        dut = _dut;
        main_time = 0L;
        is_exit = false;
#if VM_TRACE
        tfp = NULL;
#endif
    }
    void init_sim_data() {
        sim_data.inputs.clear();
        sim_data.outputs.clear();
        sim_data.signals.clear();

""")
    toplevel.inputs.foreach { case (name, width) =>
      // replaceFirst used here in case port name contains the dutName
      pushBack("inputs", "dut->" + name, width)
    }
    toplevel.outputs.foreach { case (name, width) =>
      // replaceFirst used here in case port name contains the dutName
      pushBack("outputs", "dut->" + name, width)
    }
    codeBuffer.append(
      s"""
    }
#if VM_TRACE
     void init_dump(VerilatedVcdC* _tfp) { tfp = _tfp; }
#endif
    inline bool exit() { return is_exit; }

    // required for sc_time_stamp()
    virtual inline double get_time_stamp() {
        return main_time;
    }

    private:
    $dutVerilatorClassName* dut;
    bool is_exit;
    vluint64_t main_time;
#if VM_TRACE
    VerilatedVcdC* tfp;
#endif
    virtual inline size_t put_value(VerilatorDataWrapper* &sig, uint64_t* data, bool force=false) {
        return sig->put_value(data);
    }
    virtual inline size_t get_value(VerilatorDataWrapper* &sig, uint64_t* data) {
        return sig->get_value(data);
    }
    virtual inline size_t get_chunk(VerilatorDataWrapper* &sig) {
        return sig->get_num_words();
    }
    virtual inline void start() {
    }
    virtual inline void finish() {
        dut->eval();
        is_exit = true;
    }
    virtual inline void step() {
        $clockLow
        dut->eval();
#if VM_TRACE
        if (tfp) tfp->dump(main_time);
#endif
        main_time++;
        $clockHigh
        dut->eval();
#if VM_TRACE
        if (tfp) tfp->dump(main_time);
#endif
        main_time++;
    }
    virtual inline void update() {
        // This seems to force a full eval of circuit, so registers with alternate clocks are update correctly
        dut->eval();
        // This was the original call, did not refresh registers when some  other clock transitioned
        // dut->_eval_settle(dut->__VlSymsp);
    }
};

// The following isn't strictly required unless we emit (possibly indirectly) something
// requiring a time-stamp (such as an assert).
static $dutApiClassName * _Top_api;
double sc_time_stamp () { return _Top_api->get_time_stamp(); }

// Override Verilator definition so first $$finish ends simulation
// Note: VL_USER_FINISH needs to be defined when compiling Verilator code
void vl_finish(const char* filename, int linenum, const char* hier) {
  $verilatorRunFlushCallback
  exit(0);
}

int main(int argc, char **argv, char **env) {
    Verilated::commandArgs(argc, argv);
    $dutVerilatorClassName* top = new $dutVerilatorClassName;
    std::string vcdfile = "$vcdFilePath";
    std::vector<std::string> args(argv+1, argv+argc);
    std::vector<std::string>::const_iterator it;
    for (it = args.begin() ; it != args.end() ; it++) {
        if (it->find("+waveform=") == 0) vcdfile = it->c_str()+10;
    }
#if VM_COVERAGE
    $coverageInit
#endif
#if VM_TRACE || VM_COVERAGE
    Verilated::traceEverOn(true);
#endif
#if VM_TRACE
    VL_PRINTF(\"Enabling waves..\");
    VerilatedVcdC* tfp = new VerilatedVcdC;
    top->trace(tfp, 99);
    tfp->open(vcdfile.c_str());
#endif
    $dutApiClassName api(top);
    _Top_api = &api; /* required for sc_time_stamp() */
    api.init_sim_data();
    api.init_channels();
#if VM_TRACE
    api.init_dump(tfp);
#endif
    while(!api.exit()) api.tick();
#if VM_TRACE
    if (tfp) tfp->close();
    delete tfp;
#endif
#if VM_COVERAGE
    VL_PRINTF(\"Writing Coverage..\");
    Verilated::mkdir("$targetDir/logs");
    VerilatedCov::write("$targetDir/logs/coverage.dat");
#endif
    delete top;
    exit(0);
}
"""
    )
    codeBuffer.toString()
  }
}
