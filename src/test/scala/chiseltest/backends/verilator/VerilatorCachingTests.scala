// SPDX-License-Identifier: Apache-2.0
package chiseltest.backends.verilator

import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.experimental.sanitizeFileName
import chiseltest.internal.{CachingAnnotation}
import chiseltest.tests.{PassthroughModule, StaticModule}
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File

class VerilatorCachingTests extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Testers2 with Verilator and caching"

  val annos = Seq(VerilatorBackendAnnotation, CachingAnnotation)

  it should "test static circuits" in {
    test(new StaticModule(42.U)).withAnnotations(annos) { c =>
      c.out.expect(42.U)
    }
  }

  it should "fail on poking outputs" in {
    assertThrows[UnpokeableException] {
      test(new StaticModule(42.U)).withAnnotations(annos) { c =>
        c.out.poke(0.U)
      }
    }
  }

  it should "fail on expect mismatch" in {
    assertThrows[exceptions.TestFailedException] {
      test(new StaticModule(42.U)).withAnnotations(annos) { c =>
        c.out.expect(0.U)
      }
    }
  }

  it should "fail with user-defined message" in {
    intercept[exceptions.TestFailedException] {
      test(new StaticModule(42.U)).withAnnotations(annos) { c =>
        c.out.expect(0.U, "user-defined failure message =(")
      }
    }.getMessage should include ("user-defined failure message =(")
  }

  it should "test inputless sequential circuits" in {
    test(new Module {
      val io = IO(new Bundle {
        val out = Output(UInt(8.W))
      })
      val counter = RegInit(UInt(8.W), 0.U)
      counter := counter + 1.U
      io.out := counter
    }).withAnnotations(annos) { c =>
      c.io.out.expect(0.U)
      c.clock.step()
      c.io.out.expect(1.U)
      c.clock.step()
      c.io.out.expect(2.U)
      c.clock.step()
      c.io.out.expect(3.U)
    }
  }

  it should "test combinational circuits" in {
    test(new PassthroughModule(UInt(8.W))).withAnnotations(annos) { c =>
      c.in.poke(0.U)
      c.out.expect(0.U)
      c.in.poke(42.U)
      c.out.expect(42.U)
    }
  }

  it should "test sequential circuits" in {
    test(new Module {
      val io = IO(new Bundle {
        val in = Input(UInt(8.W))
        val out = Output(UInt(8.W))
      })
      io.out := RegNext(io.in, 0.U)
    }).withAnnotations(annos) { c =>
      c.io.in.poke(0.U)
      c.clock.step()
      c.io.out.expect(0.U)
      c.io.in.poke(42.U)
      c.clock.step()
      c.io.out.expect(42.U)
    }
  }

  it should "test reset" in {
    test(new Module {
      val io = IO(new Bundle {
        val in = Input(UInt(8.W))
        val out = Output(UInt(8.W))
      })
      io.out := RegNext(io.in, 0.U)
    }).withAnnotations(annos) { c =>
      c.io.out.expect(0.U)

      c.io.in.poke(42.U)
      c.clock.step()
      c.io.out.expect(42.U)

      c.reset.poke(true.B)
      c.io.out.expect(42.U)  // sync reset not effective until next clk
      c.clock.step()
      c.io.out.expect(0.U)

      c.clock.step()
      c.io.out.expect(0.U)

      c.reset.poke(false.B)
      c.io.in.poke(43.U)
      c.clock.step()
      c.io.out.expect(43.U)
    }
  }

  it should "write vcd output when passing in a WriteVcdAnnotation" in {
    test(new Module {
      val io = IO(new Bundle {
        val a = Input(UInt(8.W))
        val b = Output(UInt(8.W))
      })
      io.b := io.a
    }).withAnnotations(annos :+ WriteVcdAnnotation) { c =>
      c.io.a.poke(1.U)
      c.io.b.expect(1.U)
      c.clock.step()
      c.io.a.poke(42.U)
      c.io.b.expect(42.U)
    }

    val testDir = new File("test_run_dir" +
      File.separator +
      sanitizeFileName(s"Testers2 with Verilator and caching should write vcd output when passing in a WriteVcdAnnotation"))

    val vcdFileOpt = testDir.listFiles.find { f =>
      f.getPath.endsWith(".vcd")
    }

    vcdFileOpt.isDefined should be(true)
    vcdFileOpt.get.delete()
  }

  it should "handle test being called twice in one case with the same module" in {
    test(new Module {
      val io = IO(new Bundle {
        val a = Input(UInt(8.W))
        val b = Output(UInt(8.W))
      })
      io.b := io.a
    }).withAnnotations(annos) { c =>
      c.io.a.poke(1.U)
      c.io.b.expect(1.U)
      c.clock.step()
      c.io.a.poke(42.U)
      c.io.b.expect(42.U)
    }

    test(new Module {
      val io = IO(new Bundle {
        val a = Input(UInt(8.W))
        val b = Output(UInt(8.W))
      })
      io.b := io.a
    }).withAnnotations(annos) { c =>
      c.io.a.poke(1.U)
      c.io.b.expect(1.U)
      c.clock.step()
      c.io.a.poke(42.U)
      c.io.b.expect(42.U)
    }
  }

  it should "handle test being called twice in one case with different modules" in {
    test(new Module {
      val io = IO(new Bundle {
        val a = Input(UInt(8.W))
        val b = Output(UInt(8.W))
      })
      io.b := io.a
    }).withAnnotations(annos) { c =>
      c.io.a.poke(1.U)
      c.io.b.expect(1.U)
      c.clock.step()
      c.io.a.poke(42.U)
      c.io.b.expect(42.U)
    }

    test(new Module {
      val io = IO(new Bundle {
        val out = Output(UInt(8.W))
      })
      val counter = RegInit(UInt(8.W), 0.U)
      counter := counter + 1.U
      io.out := counter
    }).withAnnotations(annos) { c =>
      c.io.out.expect(0.U)
      c.clock.step()
      c.io.out.expect(1.U)
      c.clock.step()
      c.io.out.expect(2.U)
      c.clock.step()
      c.io.out.expect(3.U)
    }
  }
}
