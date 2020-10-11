package scalanes.mutable

import java.nio.file.Path

import cats.effect.{Blocker, ContextShift, Sync}
import fs2.{io, Stream}
import monocle.Lens
import scalanes.mutable.Mirroring.Mirroring
import scalanes.mutable.mappers.{Mapper000, Mapper001}
import scodec.codecs.{conditional, fixedSizeBytes, ignore, uint8, vector}
import scodec.stream.StreamDecoder
import scodec.{Attempt, Decoder, Err}

import scala.language.higherKinds

class NesState(
  var ram: Vector[UInt8],
  var cpuState: CpuState,
  var ppuState: PpuState,
  var cartridge: Cartridge,
  var controllerState: ControllerState
)

object NesState {
  val ram: Lens[NesState, Vector[UInt8]]               = lens(_.ram, _.ram_=)
  val cpuState: Lens[NesState, CpuState]               = lens(_.cpuState, _.cpuState_=)
  val ppuState: Lens[NesState, PpuState]               = lens(_.ppuState, _.ppuState_=)
  val cartridge: Lens[NesState, Cartridge]             = lens(_.cartridge, _.cartridge_=)
  val controllerState: Lens[NesState, ControllerState] = lens(_.controllerState, _.controllerState_=)

  def apply(mirroring: Mirroring, cartridge: Cartridge, ref: ControllerRef): NesState =
    new NesState(
      ram = Vector.fill(0x800)(0x00),
      cpuState = CpuState(),
      ppuState = PpuState(mirroring),
      cartridge = cartridge,
      controllerState = new ControllerState(ref)
    )

  def dummy: State[NesState, NesState] = State.get

  def reset: NesState => NesState = Cpu.reset andThen Ppu.reset andThen Cartridge.reset

  val clock: NesState => NesState =
    nes => {
      val cpuCycles = nes.cpuState.cycles
      val nes1      = Cpu.clock(nes)
      val ppuCycles = (nes1.cpuState.cycles - cpuCycles) * 3
      (1L to ppuCycles).foldLeft(nes1) { case (nes, _) => Ppu.clock(nes) }
    }

  val executeFrame: NesState => NesState =
    nes => {
      val frame = nes.ppuState.frame
      var nes1  = nes
      while (nes1.ppuState.frame == frame)
        nes1 = Ppu.clock(nes1)
      nes1
    }

  private def iNesDecoder(controllerRef: ControllerRef): Decoder[NesState] =
    for {
      _           <- ignore(4 * 8)
      prgRomBanks <- uint8
      chrRomBanks <- uint8
      flags6      <- uint8
      flags7      <- uint8
      prgRamBanks <- uint8
      _           <- ignore(7 * 8)
      prgRamSize = if (prgRamBanks) prgRamBanks * 0x2000 else 0x2000
      prgRomSize = prgRomBanks * 0x4000
      chrRomSize = chrRomBanks * 0x2000
      chrRam     = if (chrRomBanks == 0) Vector.fill[UInt8](0x2000)(0x00) else Vector.empty
      mirroring  = if (flags6 & 0x1) Mirroring.Vertical else Mirroring.Horizontal
      mapperId   = (flags7 & 0xf0) | (flags6 >> 4)
      _      <- conditional(flags6 & 0x04, ignore(512 * 8))
      prgRom <- fixedSizeBytes(prgRomSize, vector(uint8))
      chrRom <- fixedSizeBytes(chrRomSize, vector(uint8))
      chrMem = if (chrRom.isEmpty) chrRam else chrRom
      cartridge <-
        if (mapperId == 0)
          Decoder.point[Cartridge](new Mapper000(prgRom, chrMem, prgRamSize))
        else if (mapperId == 1)
          Decoder.point[Cartridge](new Mapper001(prgRom, chrMem, prgRamSize))
        else
          Decoder.liftAttempt(Attempt.failure(Err(s"Unsupported mapper $mapperId!")))
    } yield NesState(mirroring, cartridge, controllerRef)

  def fromFile[F[_]: Sync: ContextShift](file: Path, controllerState: ControllerRef): Stream[F, NesState] =
    Stream
      .resource(Blocker[F])
      .flatMap { blocker =>
        io.file
          .readAll[F](file, blocker, 4096)
          .through(StreamDecoder.once(iNesDecoder(controllerState)).toPipeByte)
      }

}
