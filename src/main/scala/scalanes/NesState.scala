package scalanes

import java.nio.file.Path

import cats.Monad
import cats.data.State
import cats.effect.{Blocker, ContextShift, Sync}
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
                    freqCounter: Int) {
  def isFrameComplete: Boolean = ppuState.scanline == -1 && ppuState.cycle == 0
}

object NesState {
  val ram: Lens[NesState, Vector[UInt8]] = GenLens[NesState](_.ram)
  val cpuRegisters: Lens[NesState, CpuState] = GenLens[NesState](_.cpuState)
  val a: Lens[NesState, UInt8] = cpuRegisters composeLens GenLens[CpuState](_.a)
  val x: Lens[NesState, UInt8] = cpuRegisters composeLens GenLens[CpuState](_.x)
  val y: Lens[NesState, UInt8] = cpuRegisters composeLens GenLens[CpuState](_.y)
  val stkp: Lens[NesState, UInt8] = cpuRegisters composeLens GenLens[CpuState](_.stkp)
  val pc: Lens[NesState, UInt16] = cpuRegisters composeLens GenLens[CpuState](_.pc)
  val status: Lens[NesState, UInt8] = cpuRegisters composeLens GenLens[CpuState](_.status)
  val cycles: Lens[NesState, Int] = cpuRegisters composeLens GenLens[CpuState](_.cycles)
  val cartridge: Lens[NesState, Cartridge] = GenLens[NesState](_.cartridge)
  val ppuState: Lens[NesState, PpuState] = GenLens[NesState](_.ppuState)
  val freqCounter: Lens[NesState, Int] = GenLens[NesState](_.freqCounter)

  def dummy: State[NesState, Unit] = State.pure(())

  def getFreqCounter: State[NesState, Int] =
    State.inspect(freqCounter.get)

  def incFreqCounter: State[NesState, Unit] =
    State.modify(freqCounter.modify(a => if (a >= 2) 0 else a + 1))

  def resetFreqCounter: State[NesState, Unit] =
    State.modify(freqCounter.set(0))

  def reset: State[NesState, Unit] = for {
    _ <- Cpu.reset
    _ <- Ppu.reset
    _ <- Cartridge.reset
    _ <- resetFreqCounter
  } yield ()

  def clock: State[NesState, Unit] = for {
    _ <- Ppu.clock
    freqCounter <- getFreqCounter
    _ <- if (freqCounter == 0) Cpu.clock else dummy
    _ <- incFreqCounter
    _ <- Monad[State[NesState, *]]
      .ifM(Ppu.isVerticalBlankStarted)(
        ifTrue = Cpu.nmi,
        ifFalse = dummy
      )
  } yield ()

  def initial(mirroring: Mirroring, cartridge: Cartridge): NesState =
    NesState(
      Vector.fill(2 * 1024)(0x00),
      CpuState.initial,
      PpuState.initial(mirroring),
      cartridge,
      0
    )

  def fromString(program: String): NesState = {
    val offset = 0x8000
    val cartridge = Cartridge.fromString(program, offset)
    val s = initial(Mirroring.Horizontal, cartridge)
    pc.set(offset)(s)
  }

  private def nesFileDecoder: Decoder[NesState] = for {
    header <- ignore(4 * 8) :: uint8 :: uint8 :: uint8 :: uint8 :: uint8 :: ignore(7 * 8)
    _ :: prgRomBanks :: chrRomBanks :: flags6 :: flags7 :: prgRamBanks :: _ :: HNil = header
    prgRamSize = if (prgRamBanks) prgRamBanks * 8 * 1024 else 8 * 1024
    mirroring = if (flags6 & 0x1) Mirroring.Vertical else Mirroring.Horizontal
    mapperId = (flags7 & 0xF0) | (flags6 >> 4)
    rom <- conditional(flags6 & 0x04, ignore(512 * 8)) ::
      fixedSizeBytes(prgRomBanks * 0x4000, vector(uint8)) ::
      fixedSizeBytes(chrRomBanks * 0x2000, vector(uint8))
    _ :: prgRom :: chrRom :: HNil = rom
    cartridge <- if (mapperId == 0)
        Decoder.point(Mapper000(prgRom, chrRom, prgRamSize))
      else if (mapperId == 1)
        Decoder.point(Mapper001(prgRom, chrRom, prgRamSize))
      else
        Decoder.liftAttempt(Attempt.failure(Err(s"Unsupported mapper $mapperId!")))
  } yield NesState.initial(mirroring, cartridge)

  def fromFile[F[_] : Sync : ContextShift](file: Path): F[List[NesState]] =
    Stream.resource(Blocker[F]).flatMap { blocker =>
      io.file.readAll[F](file, blocker, 4096).through(StreamDecoder.once(nesFileDecoder).toPipeByte)
    }.compile.toList

}


