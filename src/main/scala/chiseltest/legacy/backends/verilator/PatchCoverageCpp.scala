package chiseltest.legacy.backends.verilator

import firrtl.FileUtils

import java.io.File
import java.nio.file.{Files, Path, Paths}

/** Changes the file generated by verilator to generate per instance and not per module coverage.
  * This is required in order to satisfy our generic TestCoverage interface for which
  * the simulator needs to return per instance coverage counts.
  * See: https://github.com/verilator/verilator/issues/2793
  */
object PatchCoverageCpp {
  private val CallNeedle = "VL_COVER_INSERT("
  private val CallReplacement = "CHISEL_VL_COVER_INSERT("
  private val CoverageStartNeedle = "// Coverage"

  def apply(dir: String): Unit = {
    val files = loadFiles(dir)
    files.foreach { case (cppFile, lines) =>
      replaceCoverage(cppFile, lines)
      doWrite(cppFile, lines)
    }
  }

  private def replaceCoverage(cppFile: Path, lines: Array[String]): Unit = {
    // we add our code at the beginning of the coverage section
    val coverageStart = findLine(CoverageStartNeedle, cppFile, lines)
    lines(coverageStart) += "\n" + CustomCoverInsertCode + "\n"

    // then we replace the call
    val call = findLine(CallNeedle, cppFile, lines)
    val callLine = lines(call).replaceAllLiterally(CallNeedle, CallReplacement)
    lines(call) = callLine
  }

  private def loadFiles(dir: String): Seq[(Path, Array[String])] = {
    val dFile = new File(dir)
    if (!dFile.exists() || !dFile.isDirectory) {
      error(s"Failed to find $dFile")
    }

    // find all cpp files generated by verilator
    val cppFiles = new File(dir).listFiles().filter(!_.isDirectory).filter { f =>
      val name = f.getName
      name.startsWith("V") && name.endsWith(".cpp")
    }

    // filter out files that do not contain any coverage definitions
    cppFiles.map(f => (f.toPath, FileUtils.getLines(f).toArray)).filter { case (name, lines) =>
      findLineOption(CoverageStartNeedle, lines).isDefined
    }
  }

  private def findLineOption(needle: String, lines: Iterable[String]): Option[Int] =
    lines.map(_.trim).zipWithIndex.find(_._1.startsWith(needle)).map(_._2)

  private def findLine(needle: String, filename: Path, lines: Iterable[String]): Int =
    findLineOption(needle, lines).getOrElse(error(s"Failed to find line `$needle` in $filename."))

  private def doWrite(file: Path, lines: Array[String]): Unit = os.write.over(os.Path(file.toAbsolutePath), lines.mkString("\n"))

  private def error(msg: String): Nothing = {
    throw new RuntimeException(msg + "\n" + "Please file an issue and include the output of `verilator --version`")
  }

  private val CustomCoverInsertCode =
    """#define CHISEL_VL_COVER_INSERT(countp, ...) \
      |    VL_IF_COVER(VerilatedCov::_inserti(countp); VerilatedCov::_insertf(__FILE__, __LINE__); \
      |                chisel_insertp("hier", name(), __VA_ARGS__))
      |
      |#ifdef VM_COVERAGE
      |static void chisel_insertp(
      |  const char* key0, const char* valp0, const char* key1, const char* valp1,
      |  const char* key2, int lineno, const char* key3, int column,
      |  const char* key4, const std::string& hier_str,
      |  const char* key5, const char* valp5, const char* key6, const char* valp6,
      |  const char* key7 = nullptr, const char* valp7 = nullptr) {
      |
      |    std::string val2str = vlCovCvtToStr(lineno);
      |    std::string val3str = vlCovCvtToStr(column);
      |    VerilatedCov::_insertp(
      |        key0, valp0, key1, valp1, key2, val2str.c_str(),
      |        key3, val3str.c_str(), key4, hier_str.c_str(),
      |        key5, valp5, key6, valp6, key7, valp7,
      |        // turn on per instance cover points
      |        "per_instance", "1");
      |}
      |#endif
      |""".stripMargin
}
