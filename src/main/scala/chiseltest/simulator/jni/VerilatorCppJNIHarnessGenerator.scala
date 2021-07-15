package chiseltest.simulator.jni

import chiseltest.simulator.TopmoduleInfo

/** Generates the Module specific verilator harness cpp file for verilator compilation.
  *  This version generates a harness that can be called into through the JNI.
  */
private[chiseltest] object VerilatorCppJNIHarnessGenerator {
  def codeGen(
    toplevel:     TopmoduleInfo,
    vcdFilePath:  String,
    targetDir:    String,
    majorVersion: Int,
    minorVersion: Int
  ): String = {
    val codeBuffer = new StringBuilder

    def pushBack(vector: String, pathName: String, width: BigInt): Unit = {
      if (width == 0) {
        // Do nothing- 0 width wires are removed
      } else if (width <= 8) {
        codeBuffer.append(s"        s->sim_data.$vector.push_back(new VerilatorCData(&($pathName)));\n")
      } else if (width <= 16) {
        codeBuffer.append(s"        s->sim_data.$vector.push_back(new VerilatorSData(&($pathName)));\n")
      } else if (width <= 32) {
        codeBuffer.append(s"        s->sim_data.$vector.push_back(new VerilatorIData(&($pathName)));\n")
      } else if (width <= 64) {
        codeBuffer.append(s"        s->sim_data.$vector.push_back(new VerilatorQData(&($pathName)));\n")
      } else {
        val numWords = (width - 1) / 32 + 1
        codeBuffer.append(s"        s->sim_data.$vector.push_back(new VerilatorWData($pathName, $numWords));\n")
      }
    }

    val dutName = toplevel.name
    val dutVerilatorClassName = "V" + dutName

    val ApiPrefix = "JNICALL Java_chiseltest_simulator_jni_TesterSharedLib"

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
#include "${dutVerilatorClassName}.h"
#include "verilated.h"
#include "veri_api.h"
#if VM_TRACE
#include "verilated_vcd_c.h"
#endif
#include <iostream>

// Override Verilator definition so first $$finish ends simulation
// Note: VL_USER_FINISH needs to be defined when compiling Verilator code
void vl_finish(const char* filename, int linenum, const char* hier) {
  Verilated::flushCall();
  exit(0);
}

#ifdef INCLUDE_MAIN
#else /* INCLUDE_MAIN */
#include <jni.h>

struct sim_state {
  $dutVerilatorClassName* dut;
  VerilatedVcdC* tfp;
  vluint64_t main_time;
  sim_data_t<VerilatorDataWrapper*> sim_data;

  sim_state() :
    dut(new $dutVerilatorClassName),
    tfp(nullptr),
    main_time(0)
  {
    // std::cout << "Allocating! " << ((long long) dut) << std::endl;
  }

};


extern "C" {

jfieldID getPtrId(JNIEnv *env, jobject obj) {
  jclass c = env->GetObjectClass(obj);
  jfieldID id = env->GetFieldID(c, "state", "J");
  env->DeleteLocalRef(c);

  return id;
}

sim_state* get_state(JNIEnv *env, jobject obj) {
  static sim_state* cached = NULL;
  if (cached == NULL) {
    cached = (sim_state*) env->GetLongField(obj, getPtrId(env, obj));
  }
  return cached;
}

JNIEXPORT void JNICALL ${ApiPrefix}_sim_1init(JNIEnv *env, jobject obj) {
  sim_state *s = new sim_state();

  env->SetLongField(obj, getPtrId(env, obj), (jlong)s);
  std::string vcdfile = "${vcdFilePath}";

#if VM_COVERAGE
    $coverageInit
#endif
#if VM_TRACE || VM_COVERAGE
    Verilated::traceEverOn(true);
#endif
#if VM_TRACE
    Verilated::traceEverOn(true);
    // VL_PRINTF(\"Enabling waves..\\n\");
    s->tfp = new VerilatedVcdC;
    // s->main_time = 0;
    s->dut->trace(s->tfp, 99);
    s->tfp->open(vcdfile.c_str());
#endif
  s->sim_data.inputs.clear();
  s->sim_data.outputs.clear();
  s->sim_data.signals.clear();

""")
    var signalMapCnt = 0
    toplevel.inputs.foreach { case (name, width) =>
      val mapName = name // TODO: what is the right mapName?
      pushBack("signals", "s->dut->" + name, width)
      codeBuffer.append(s"""        s->sim_data.signal_map["$mapName"] = $signalMapCnt;""")
      signalMapCnt += 1
    }
    toplevel.outputs.foreach { case (name, width) =>
      val mapName = name // TODO: what is the right mapName?
      pushBack("signals", "s->dut->" + name, width)
      codeBuffer.append(s"""        s->sim_data.signal_map["$mapName"] = $signalMapCnt;""")
      signalMapCnt += 1
    }
    codeBuffer.append(s"""
}

JNIEXPORT void ${ApiPrefix}_step(JNIEnv *env, jobject obj) {
  sim_state *s = get_state(env, obj);

  s->dut->clock = 0;
  s->dut->eval();
#if VM_TRACE
  if (s->tfp) s->tfp->dump(s->main_time);
#endif
  s->main_time++;
  s->dut->clock = 1;
  s->dut->eval();
#if VM_TRACE
  if (s->tfp) s->tfp->dump(s->main_time);
#endif
  s->main_time++;
}

JNIEXPORT void ${ApiPrefix}_start(JNIEnv *env, jobject obj) {
  sim_state *s = get_state(env, obj);

  s->dut->reset = 0; // for backwards compatibility reasons
}

JNIEXPORT void ${ApiPrefix}_update(JNIEnv *env, jobject obj) {
  sim_state *s = get_state(env, obj);

  s->dut->eval();
}

JNIEXPORT void ${ApiPrefix}_finish(JNIEnv *env, jobject obj) {
  sim_state *s = get_state(env, obj);

  s->dut->eval();

#if VM_TRACE
    if (s->tfp) s->tfp->close();
    delete s->tfp;
#endif
#if VM_COVERAGE
    Verilated::mkdir("$targetDir/logs");
    VerilatedCov::write("$targetDir/logs/coverage.dat");
#endif
}

JNIEXPORT void ${ApiPrefix}_resetCoverage(JNIEnv *env, jobject obj) {
  VerilatedCov::zero();
}

JNIEXPORT void ${ApiPrefix}_writeCoverage(JNIEnv *env, jobject obj, jstring filename) {
  const char *c_str = env->GetStringUTFChars(filename, NULL);
  VerilatedCov::write(c_str);
}

JNIEXPORT void ${ApiPrefix}_poke(JNIEnv *env, jobject obj, jint id, jlong value) {
  sim_state *s = get_state(env, obj);

  VerilatorDataWrapper *sig = s->sim_data.signals[id];
  if (!sig) {
    std::cerr << "Cannot find the object of id = " << id << std::endl;
    ${ApiPrefix}_finish(env, obj);
    // TODO what?
  } else {
    // std::cout << "Poking signal " << id << " with value " << value << std::endl;
  }
  uint64_t toput = value;
  sig->put_value(&toput);
}

JNIEXPORT jlong ${ApiPrefix}_peek(JNIEnv *env, jobject obj, jint id) {
  sim_state *s = get_state(env, obj);

  VerilatorDataWrapper *sig = s->sim_data.signals[id];
  if (!sig) {
    std::cerr << "Cannot find the object of id = " << id << std::endl;
    ${ApiPrefix}_finish(env, obj);
    // TODO what?
  } else {
    // std::cout << "Peeking signal " << id << std::endl;
  }
  uint64_t toret;
  sig->get_value(&toret);
  return toret;
}

JNIEXPORT jint ${ApiPrefix}_getid(JNIEnv *env, jobject obj, jstring jniPath) {
  sim_state *s = get_state(env, obj);

  const char *path = env->GetStringUTFChars(jniPath, NULL);

  std::map<std::string, size_t>::iterator it;

  it = s->sim_data.signal_map.find(path);
  jint id = -1;

  if (it != s->sim_data.signal_map.end()) {
    id = it->second;
    // std::cout << "Found " << path << " with id " << id << std::endl;
  } else {
    // id = search(path);
    // if (id < 0) {
      std::cerr << "Cannot find the object " << path << std::endl;
    // }
  }

  env->ReleaseStringUTFChars(jniPath, path);

  return id;
}

}
#endif /* INCLUDE_MAIN */
""")
    codeBuffer.toString()
  }
}

object JNIUtils {
  def javaHome: String = System.getProperty("java.home") match {
    case s: String if s.endsWith("/jre") => s.dropRight(4)
    case s: String                       => s
  }
  def osIncludeName: String = System.getProperty("os.name") match {
    case "Mac OS X" => "darwin"
    case "Linux"    => "linux"
    case s: String => s
  }

  /** additional ccFlags that are required in order to compile into a library that can be loaded with JNI */
  def ccFlags: Seq[String] = Seq(
    "-fPIC",
    "-shared",
    s"-I$javaHome/include",
    s"-I$javaHome/include/$osIncludeName"
  )
}
