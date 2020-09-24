package scalanes.mutable

import java.nio.file.Path

import cats.effect.{Blocker, ContextShift, Sync}
import fs2.{Stream, io}
import monocle.Lens
import scalanes.mutable.CpuFlags.CpuFlags
import scalanes.mutable.Mirroring.Mirroring
import scalanes.mutable.mappers.{Mapper000, Mapper001}
import scodec.codecs.{conditional, fixedSizeBytes, ignore, uint8, vector}
import scodec.stream.StreamDecoder
import scodec.{Attempt, Decoder, Err}

import scala.language.higherKinds

class CpuState(
  var a: UInt8,
  var x: UInt8,
  var y: UInt8,
  var stkp: UInt8,
  var pc: UInt16,
  var status: UInt8,
  var cycles: Int,
  var haltAt: UInt16
) {

  def getFlag(flag: CpuFlags): Boolean = status & flag.bit

  def isValid: Boolean =
    isValidUInt8(a) & isValidUInt8(x) & isValidUInt8(y) & isValidUInt8(y) & isValidUInt8(stkp) &
    isValidUInt16(pc) & isValidUInt8(status) & isValidUInt16(haltAt)
}

object CpuState {
  val a: Lens[CpuState, UInt8] = lens(_.a, _.a_=)
  val x: Lens[CpuState, UInt8] = lens(_.x, _.x_=)
  val y: Lens[CpuState, UInt8] = lens(_.x, _.y_=)
  val stkp: Lens[CpuState, UInt8] = lens(_.stkp, _.stkp_=)
  val pc: Lens[CpuState, UInt16] = lens(_.pc, _.pc_=)
  val status: Lens[CpuState, UInt8] = lens(_.status, _.status_=)
  val cycles: Lens[CpuState, Int] = lens(_.cycles, _.cycles_=)
  val haltAt: Lens[CpuState, UInt16] = lens(_.haltAt, _.haltAt_=)

  def initial: CpuState =
    new CpuState(
     0x00,
     0x00,
     0x00,
     0xFD,
     0x0000,
     0x00 | CpuFlags.U.bit | CpuFlags.I.bit,
     0,
     0xFFFF
    )

}

object CpuFlags extends Enumeration {
  protected case class Val(bit: Int) extends super.Val
  type CpuFlags = Val
  val C: CpuFlags = Val(1 << 0)
  val Z: CpuFlags = Val(1 << 1)
  val I: CpuFlags = Val(1 << 2)
  val D: CpuFlags = Val(1 << 3)
  val B: CpuFlags = Val(1 << 4)
  val U: CpuFlags = Val(1 << 5)
  val V: CpuFlags = Val(1 << 6)
  val N: CpuFlags = Val(1 << 7)
}

class NesState(
  // 2KB internal RAM
  var ram: Vector[UInt8],
  var cpuState: CpuState,
  var ppuState: PpuState,
  var cartridge: Cartridge,
  var controllerState: ControllerState
)

object NesState {
  val ram: Lens[NesState, Vector[UInt8]] = lens(_.ram, _.ram_=)
  val cpuState: Lens[NesState, CpuState] = lens(_.cpuState, _.cpuState_=)
  val ppuState: Lens[NesState, PpuState] = lens(_.ppuState, _.ppuState_=)
  val cartridge: Lens[NesState, Cartridge] = lens(_.cartridge, _.cartridge_=)
  val controllerState: Lens[NesState, ControllerState] = lens(_.controllerState, _.controllerState_=)

  def initial(mirroring: Mirroring, cartridge: Cartridge, ref: ControllerRef): NesState =
    new NesState(
      Vector.fill(0x800)(0x00),
      CpuState.initial,
      PpuState.initial(mirroring),
      cartridge,
      new ControllerState(ref)
    )

  def dummy: State[NesState, NesState] = State.get

  def reset: State[NesState, Unit] = for {
    _ <- Cpu.reset
    _ <- Ppu.reset
    _ <- Cartridge.reset
  } yield ()

  def clock(counter: Int, scanline: Int, cycle: Int): Option[State[NesState, NesState]] = {
    val ppuClock = Ppu.clock(scanline, cycle)
    val ppuReady = ppuClock.nonEmpty
    val cpuReady = (counter % 3) == 1
    val nmiReady = scanline == 241 && cycle == 2

    (ppuReady, cpuReady, nmiReady) match {
      case (true, true, true) =>
        Option(
          ppuClock.get.flatMap { nes =>
            if (Ppu.isNmiReady(scanline, cycle, nes.ppuState)) Cpu.nmi
            else dummy
          }.flatMap(_ => Cpu.clock)
        )

      case (true, true, false) =>
        Option(ppuClock.get *> Cpu.clock)

      case (true, false, true) =>
        Option(
          ppuClock.get.flatMap { nes =>
            if (Ppu.isNmiReady(scanline, cycle, nes.ppuState)) Cpu.nmi
            else dummy
          }
        )

      case (_, false, false) =>
        ppuClock

      case (false, true, true) =>
        Option(
          Cpu.clock.flatMap { nes =>
            if (Ppu.isNmiReady(scanline, cycle, nes.ppuState)) Cpu.nmi
            else dummy
          }
        )

      case (false, true, false) =>
        Option(Cpu.clock)

      case (false, false, true) =>
        Option(
          State.get[NesState].flatMap { nes =>
            if (Ppu.isNmiReady(scanline, cycle, nes.ppuState)) Cpu.nmi
            else dummy
          }
        )

    }
  }

  val frameTicks: Seq[(Int, Int, Int)] =
    (
      for {
        scanline <- -1 to 260
        cycle    <-  0 to 340
        if scanline != 0 || cycle != 0
      } yield (scanline, cycle)
      ).zipWithIndex.map { case ((scanline, cycle), counter) =>
      (counter, scanline, cycle)
    }

  val executeFrame: State[NesState, NesState] =
    frameTicks
      .flatMap { case (counter, scanline, cycle) =>
        clock(counter, scanline, cycle)
      }
      .reduce(_ *> _)

  private def nesFileDecoder(controllerRef: ControllerRef): Decoder[NesState] =
    for {
      _           <- ignore(4 * 8)
      prgRomBanks <- uint8
      chrRomBanks <- uint8
      flags6      <- uint8
      flags7      <- uint8
      prgRamBanks <- uint8
      _           <- ignore(7 * 8)
      prgRamSize  =  if (prgRamBanks) prgRamBanks * 0x2000 else 0x2000
      prgRomSize  =  prgRomBanks * 0x4000
      chrRomSize  =  chrRomBanks * 0x2000
      chrRam      =  if (chrRomBanks == 0) Vector.fill[UInt8](0x2000)(0x00) else Vector.empty
      mirroring   =  if (flags6 & 0x1) Mirroring.Vertical else Mirroring.Horizontal
      mapperId    =  (flags7 & 0xF0) | (flags6 >> 4)
      _           <- conditional(flags6 & 0x04, ignore(512 * 8))
      prgRom      <- fixedSizeBytes(prgRomSize, vector(uint8))
      chrRom      <- fixedSizeBytes(chrRomSize, vector(uint8))
      chrMem      =  if (chrRom.isEmpty) chrRam else chrRom
      cartridge   <- if (mapperId == 0)
                       Decoder.point[Cartridge](new Mapper000(prgRom, chrMem, prgRamSize))
                     else if (mapperId == 1)
                       Decoder.point[Cartridge](new Mapper001(prgRom, chrMem, prgRamSize))
                     else
                       Decoder.liftAttempt(Attempt.failure(Err(s"Unsupported mapper $mapperId!")))
    } yield NesState.initial(mirroring, cartridge, controllerRef)

  def fromFile[F[_]: Sync: ContextShift](file: Path, controllerState: ControllerRef): Stream[F, NesState] =
    Stream
      .resource(Blocker[F])
      .flatMap { blocker =>
        io.file
          .readAll[F](file, blocker, 4096)
          .through(StreamDecoder.once(nesFileDecoder(controllerState)).toPipeByte)
      }

}
