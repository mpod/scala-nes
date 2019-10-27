package scalanes

import java.nio.file.Path

import cats.effect.{Blocker, ContextShift, Sync}
import cats.implicits._
import fs2.{Stream, io}
import monocle.Lens
import monocle.macros.GenLens
import scalanes.Mirroring.Mirroring
import scalanes.mappers.{Mapper000, Mapper001}
import scodec.{Attempt, Decoder, Err}
import scodec.codecs.{conditional, fixedSizeBytes, ignore, uint8, vector}
import scodec.stream.StreamDecoder
import shapeless.{::, HNil}

import scala.language.higherKinds

case class NesState(ram: Vector[UInt8], cpuRegisters: CpuState, ppuState: PpuState, cartridge: Cartridge) {
  def isFrameComplete: Boolean = ppuState.scanline == -1 && ppuState.cycle == 0
}

object NesState {
  val ram: Lens[NesState, Vector[UInt8]] = GenLens[NesState](_.ram)
  val cpuRegisters: Lens[NesState, CpuState] = GenLens[NesState](_.cpuRegisters)
  val a: Lens[NesState, UInt8] = cpuRegisters composeLens GenLens[CpuState](_.a)
  val x: Lens[NesState, UInt8] = cpuRegisters composeLens GenLens[CpuState](_.x)
  val y: Lens[NesState, UInt8] = cpuRegisters composeLens GenLens[CpuState](_.y)
  val stkp: Lens[NesState, UInt8] = cpuRegisters composeLens GenLens[CpuState](_.stkp)
  val pc: Lens[NesState, UInt16] = cpuRegisters composeLens GenLens[CpuState](_.pc)
  val status: Lens[NesState, UInt8] = cpuRegisters composeLens GenLens[CpuState](_.status)
  val cycles: Lens[NesState, Int] = cpuRegisters composeLens GenLens[CpuState](_.cycles)
  val cartridge: Lens[NesState, Cartridge] = GenLens[NesState](_.cartridge)
  val ppuState: Lens[NesState, PpuState] = GenLens[NesState](_.ppuState)

  def initial(mirroring: Mirroring, cartridge: Cartridge): NesState =
    NesState(
      Vector.fill(2 * 1024)(0x00),
      CpuState.initial,
      PpuState.initial(mirroring),
      cartridge
    )

  def fromString(program: String): NesState = {
    val offset = 0x8000
    val cartridge = Cartridge.fromString(program, offset)
    val s = initial(Mirroring.Horizontal, cartridge)
    pc.set(offset)(s)
  }

  private def nesFileDecoder: Decoder[NesState] = for {
    header <- ignore(4) :: uint8 :: uint8 :: uint8 :: uint8 :: uint8 :: ignore(7)
    _ :: prgRomBanks :: chrRomBanks :: flags6 :: flags7 :: prgRamBanks :: _ :: HNil = header
    prgRamSize = if (prgRamBanks) prgRamBanks * 8 * 1024 else 8 * 1024
    mirroring = if (flags6 & 0x1) Mirroring.Vertical else Mirroring.Horizontal
    mapperId = (flags7 & 0x0F) | (flags6 >> 4)
    rom <- conditional(flags6 & 0x04, ignore(512)) ::
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

  def fromFile[F[_] : Sync : ContextShift](file: Path): F[NesState] =
    Stream.resource(Blocker[F]).flatMap { blocker =>
      io.file.readAll[F](file, blocker, 4096).through(StreamDecoder.once(nesFileDecoder).toPipeByte)
    }.compile.toList.map(_.head)

}

// TODO: Move to the Cpu
case class CpuState(a: UInt8, x: UInt8, y: UInt8, stkp: UInt8, pc: UInt16, status: UInt8, cycles: Int)

object CpuState {
  val initial: CpuState = CpuState(0x00, 0x00, 0x00, 0xFD, 0x0000, 0x00 | CpuFlags.U.bit, 0)
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

