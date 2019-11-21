package scalanes

import java.nio.file.Path

import cats.effect.{Blocker, ContextShift, Sync}
import cats.implicits._
import fs2.{Stream, io}
import monocle.Lens
import monocle.macros.GenLens
import scalanes.Mirroring.Mirroring
import scalanes.mappers.{Mapper000, Mapper001}
import scodec.codecs.{conditional, fixedSizeBytes, ignore, uint8, vector}
import scodec.stream.StreamDecoder
import scodec.{Attempt, Decoder, Err}
import shapeless.{::, HNil}

import scala.language.higherKinds

case class NesState(ram: Vector[UInt8],
                    cpuState: CpuState,
                    ppuState: PpuState,
                    cartridge: Cartridge,
                    controllerState: ControllerState)

object NesState {
  val ram: Lens[NesState, Vector[UInt8]] = GenLens[NesState](_.ram)
  val cpuState: Lens[NesState, CpuState] = GenLens[NesState](_.cpuState)
  val a: Lens[NesState, UInt8] = cpuState composeLens GenLens[CpuState](_.a)
  val x: Lens[NesState, UInt8] = cpuState composeLens GenLens[CpuState](_.x)
  val y: Lens[NesState, UInt8] = cpuState composeLens GenLens[CpuState](_.y)
  val stkp: Lens[NesState, UInt8] = cpuState composeLens GenLens[CpuState](_.stkp)
  val pc: Lens[NesState, UInt16] = cpuState composeLens GenLens[CpuState](_.pc)
  val status: Lens[NesState, UInt8] = cpuState composeLens GenLens[CpuState](_.status)
  val cycles: Lens[NesState, Int] = cpuState composeLens GenLens[CpuState](_.cycles)
  val cartridge: Lens[NesState, Cartridge] = GenLens[NesState](_.cartridge)
  val ppuState: Lens[NesState, PpuState] = GenLens[NesState](_.ppuState)
  val controllerState: Lens[NesState, ControllerState] = GenLens[NesState](_.controllerState)

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

  val executeFrameCpu: State[NesState, NesState] =
    frameTicks
      // Assumption is that CPU spends most of its time on waiting for vertical blank period
      .filterNot { case (_, scanline, _) => scanline >= 0 && scanline < 100 }
      .foldLeft(Cpu.clock) { (state, tick) =>
        val (counter, scanline, cycle) = tick
        if (counter != 0 && counter % 3 == 0)
          if (scanline == 241 && cycle == 2)
            state.flatMap { ns =>
              if (ns.ppuState.registers.ctrl.nmiMode == NmiMode.On)
                Cpu.nmi *> Cpu.clock
              else
                Cpu.clock
            }
          else
            state *> Cpu.clock
        else if (scanline == 241 && cycle == 2)
          state.flatMap { ns =>
            if (ns.ppuState.registers.ctrl.nmiMode == NmiMode.On)
              Cpu.nmi
            else
              State.get
          }
        else if (scanline == 241 && cycle == 1) {
          state *> Ppu.setVerticalBlankS(true)
        } else if (scanline == -1 && cycle == 1) {
          state *> Ppu.setVerticalBlankS(false)
        } else
          state
      }

  def executeFramePpu: State[NesState, NesState] =
    frameTicks
      // Skip PPU useless cycles
      .filterNot { case (_, scanline, _) => scanline >= 240 && scanline <= 260 }
      .foldLeft(State.get[NesState]) { case (state, (_, scanline, cycle)) =>
        state *> Ppu.clock(scanline, cycle).get
      }

  val executeFrame: State[NesState, NesState] =
    frameTicks
      .flatMap { case (counter, scanline, cycle) => clock(counter, scanline, cycle) }
      .foldLeft(State.get[NesState])(_ *> _)

  def initial(mirroring: Mirroring, cartridge: Cartridge, ref: ControllerRef): NesState =
    NesState(
      Vector.fill(0x800)(0x00),
      CpuState.initial,
      PpuState.initial(mirroring),
      cartridge,
      ControllerState(ref)
    )

  private def nesFileDecoder(controllerRef: ControllerRef): Decoder[NesState] = for {
    header     <- ignore(4 * 8) :: uint8 :: uint8 :: uint8 :: uint8 :: uint8 :: ignore(7 * 8)
    _ :: prgRomBanks :: chrRomBanks :: flags6 :: flags7 :: prgRamBanks :: _ :: HNil = header
    prgRamSize =  if (prgRamBanks) prgRamBanks * 0x2000 else 0x2000
    prgRomSize =  prgRomBanks * 0x4000
    chrRomSize =  chrRomBanks * 0x2000
    chrRam     =  if (chrRomBanks == 0) Vector.fill[UInt8](0x2000)(0x00) else Vector.empty
    mirroring  =  if (flags6 & 0x1) Mirroring.Vertical else Mirroring.Horizontal
    mapperId   =  (flags7 & 0xF0) | (flags6 >> 4)
    rom        <- conditional(flags6 & 0x04, ignore(512 * 8)) ::
                  fixedSizeBytes(prgRomSize, vector(uint8)) ::
                  fixedSizeBytes(chrRomSize, vector(uint8))
    _ :: prgRom :: chrRom :: HNil = rom
    chrMem     =  if (chrRom.isEmpty) chrRam else chrRom
    cartridge  <- if (mapperId == 0)
        Decoder.point(Mapper000(prgRom, chrMem, prgRamSize))
      else if (mapperId == 1)
        Decoder.point(Mapper001(prgRom, chrMem, prgRamSize))
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


