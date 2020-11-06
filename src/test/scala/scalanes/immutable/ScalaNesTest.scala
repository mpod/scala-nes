package scalanes.immutable

import java.nio.file.Paths

import cats.effect.concurrent.Ref
import cats.effect.{Blocker, ContextShift, IO}
import fs2.{io, text, Pure, Stream}
import org.scalatest.Matchers
import org.scalatest.flatspec.AnyFlatSpec

import scala.concurrent.ExecutionContext

class ScalaNesTest extends AnyFlatSpec with Matchers {

  implicit val contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  def compare(logLine: String, s: NesState): Boolean = {
    val op = Cpu.getPc.flatMap(pc => Cpu.cpuRead(pc)).runA(s).unsafeRunSync()
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

  "ScalaNes" should "pass the nestest test" in {
    val nestestRom = Paths.get(getClass.getResource("/nestest.nes").toURI)
    val nestestLog = Paths.get(getClass.getResource("/nestest.log").toURI)

    val decodedNesRom = (
      for {
        controller <- Ref.of[IO, UInt8](0x00)
        loaded     <- NesState.fromFile[IO](nestestRom, controller).compile.toList
      } yield loaded
    ).unsafeRunSync()
    decodedNesRom should have size 1
    val initialNesState = NesState.reset.flatMap(_ => Cpu.setPc(0xc000)).runS(decodedNesRom.head).unsafeRunSync()

    val nesStates: Stream[Pure, NesState] = Stream.emit(initialNesState) ++ Stream.unfold(initialNesState) { s =>
      val next = Cpu.executeNextInstr.runS(s).unsafeRunSync()
      Option((next, next))
    }

    val nesTestLog: Stream[IO, String] = Stream.resource(Blocker[IO]).flatMap { blocker =>
      io.file
        .readAll[IO](nestestLog, blocker, 4096)
        .through(text.utf8Decode)
        .through(text.lines)
    }

    val result = (nesStates zip nesTestLog)
      .filter { case (s, line) => !compare(line, s) }
      .take(1)
      .compile
      .toList
      .unsafeRunSync()

    if (result.nonEmpty) {
      println(s"Failed at: ${result.head._2}")
    }
    result shouldBe empty

  }
}