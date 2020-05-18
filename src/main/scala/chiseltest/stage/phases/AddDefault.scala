package chiseltest.stage.phases

import java.io.File

import chiseltest.stage._
import firrtl.AnnotationSeq
import firrtl.annotations.Annotation
import firrtl.options.{Phase, PreservesAll, TargetDirAnnotation}

/** [[AddDefault]] is a shared [[Phase]] between different backend,
  * it will add:
  * [[SimulatorBackendAnnotation]]
  * default to be treadle.
  * if backend is `vcs`, will add default `LM_LICENSE_FILE` based on `LicenseAnnotation` or find from system env.
  * if backend is `verilator` and [[WaveFormAnnotation]] is `fsdb`, it will add `LM_LICENSE_FILE` too,
  * and add a Vcd2FsdbBinaryAnnotation (@todo)
  * [[SimulatorBinary]]:
  * default to be same with name in [[SimulatorBackendAnnotation(name)]], which will be lookup by PATH env.
  * [[WaveFormAnnotation]]:
  * default to be none
  *
  * For the better compatibility to verilator/vcs, [[TargetDirAnnotation]] will be convert absolute path.
  * */
class AddDefault extends Phase with ChiselTesterAnnotationHelper with PreservesAll[Phase] {
  def addDefaultBackend(annos: AnnotationSeq): SimulatorBackendAnnotation = annos.collectFirst {
    case b: SimulatorBackendAnnotation => b
    case VerilatorBackendAnnotation() => SimulatorBackendAnnotation("verilator")
    case VcsBackendAnnotation() => SimulatorBackendAnnotation("vcs")
    case TreadleBackendAnnotation() => SimulatorBackendAnnotation("treadle")
  }.getOrElse(SimulatorBackendAnnotation("treadle"))

  def addDefaultBinary(annos: AnnotationSeq): SimulatorBinary = annos.collectFirst {
    case f: SimulatorBinary => f
  }.getOrElse {
    SimulatorBinary(annos.collectFirst { case SimulatorBackendAnnotation(b) => b }.get)
  }

  def addWaveForm(annos: AnnotationSeq): WaveFormAnnotation = annos.collectFirst {
    case w: WaveFormAnnotation => w
    case WriteVcdAnnotation() => WaveFormAnnotation("vcd")
  }.getOrElse(WaveFormAnnotation("none"))

  def convertAbsoluteTargetDir(annos: AnnotationSeq): Seq[Annotation] = annos.map {
    case TargetDirAnnotation(f) => TargetDirAnnotation(new File(f).getAbsolutePath)
    case a => a
  }

  def transform(a: AnnotationSeq): AnnotationSeq = {
    val backendAnnotation = addDefaultBackend(a)
    val annotationPerBackend = backendAnnotation match {
      case SimulatorBackendAnnotation("vcs") =>
        val license = getLicense(a)
        val enableCache = a.collectFirst { case a: EnableCache => a }.getOrElse {
          EnableCache(true)
        }
        val vcd = addWaveForm(a) match {
          case p@WaveFormAnnotation("vcd") => p
          case _ => WaveFormAnnotation("none")
        }
        val binary = addDefaultBinary(a)
        Seq(enableCache, vcd, backendAnnotation, license, binary)
      case SimulatorBackendAnnotation("verilator") =>
        val waveForm = addWaveForm(a) match {
          case p@WaveFormAnnotation("fsdb") => Seq(p, getLicense(a))
          case p: WaveFormAnnotation => Seq(p)
        }
        val binary = addDefaultBinary(a)
        Seq(backendAnnotation, binary) ++ waveForm
      case SimulatorBackendAnnotation("treadle") =>
        Seq(backendAnnotation)
    }
    convertAbsoluteTargetDir(a) ++ annotationPerBackend
  }
}