package scalanes

import java.nio.file.Paths
import cats.effect.{Blocker, ContextShift, IO}
import fs2.concurrent.Queue
import fs2.{io, text, Stream}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.ExecutionContext

class MutableScalaNesTest extends AnyFlatSpec with Matchers {

  implicit val contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  def compare(logLine: String, s: NesState): Boolean = {
    val op = Cpu.cpuRead(s.cpuState.pc).runA(s)
    logLine.contains(s"${hex(s.cpuState.pc, 4)}  ${hex(op, 2)} ") &&
    logLine.contains(s" A:${hex(s.cpuState.a, 2)}") &&
    logLine.contains(s" X:${hex(s.cpuState.x, 2)}") &&
    logLine.contains(s" Y:${hex(s.cpuState.y, 2)}") &&
    (
      logLine.contains(s" P:${hex(s.cpuState.status, 2)}") ||
        logLine.contains(s" P:${hex(s.cpuState.status & 0xef, 2)}")
    ) &&
    logLine.contains(s" SP:${hex(s.cpuState.stkp, 2)}")
  }

  "Mutable ScalaNes" should "pass the nestest test" in {
    val nestestRom = Paths.get(getClass.getResource("/nestest.nes").toURI)
    val nestestLog = Paths.get(getClass.getResource("/nestest.log").toURI)
    val queue      = Queue.circularBuffer[IO, Byte](1).unsafeRunSync()

    val decodedNesRom = NesState.fromFile[IO](nestestRom, queue).compile.toList.unsafeRunSync()
    decodedNesRom should have size 1
    val initialNesState = {
      val nes1 = decodedNesRom.head
      val nes2 = NesState.reset(nes1)
      Cpu.setPc(0xc000)(nes2)
    }

    var nesTestLog: List[String] = Stream
      .resource(Blocker[IO])
      .flatMap { blocker =>
        io.file
          .readAll[IO](nestestLog, blocker, 4096)
          .through(text.utf8Decode)
          .through(text.lines)
      }
      .compile
      .toList
      .unsafeRunSync()

    var next = initialNesState
    while (nesTestLog.nonEmpty && compare(nesTestLog.head, next)) {
      next = Cpu.clock(next)
      nesTestLog = nesTestLog.tail
    }
    if (nesTestLog.nonEmpty) {
      val pc   = hex(next.cpuState.pc, 4)
      val op   = hex(Cpu.cpuRead(next.cpuState.pc).runA(next), 2)
      val a    = hex(next.cpuState.a, 2)
      val x    = hex(next.cpuState.x, 2)
      val y    = hex(next.cpuState.y, 2)
      val p    = hex(next.cpuState.status, 2)
      val stkp = hex(next.cpuState.stkp, 2)
      println(s"$pc $op A:$a X:$x Y:$y P:$p SP: $stkp   <-->   ${nesTestLog.head}")
    }
    nesTestLog shouldBe empty
  }
}
