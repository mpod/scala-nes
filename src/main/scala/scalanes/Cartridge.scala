package scalanes

import java.nio.file.Path

import cats.data.State
import cats.effect.{Blocker, ContextShift, _}
import cats.implicits._
import fs2.{Stream, io}
import scalanes.Mirroring.Mirroring
import scalanes.mappers.{Mapper000, Mapper001}
import scodec.codecs._
import scodec.stream.StreamDecoder
import scodec.{Attempt, Decoder, Err}
import shapeless._

import scala.language.higherKinds

object Cartridge {

  implicit class CartridgeOps[A](val a: State[Cartridge, A]) extends AnyVal {
    def toNesState: State[NesState, A] = a.transformS(
      NesState.cartridge.get,
      (nesState, cartridge) => NesState.cartridge.set(cartridge)(nesState)
    )
  }

  def empty: Cartridge = Mapper000(
    Vector.fill(32 * 1024)(0x00),
    Vector.fill(8 * 1024)(0x00),
    0,
    Mirroring.Horizontal
  )

  def cpuRead(address: UInt16): State[NesState, UInt8] =
    State.inspect(_.cartridge.prgRead(address))

  def cpuWrite(address: UInt16, d: UInt8): State[NesState, Unit] =
    State.modify[Cartridge](_.prgWrite(address, d)).toNesState

  def ppuRead(address: UInt16): State[NesState, UInt8] =
    State.inspect(_.cartridge.chrRead(address))

  def ppuWrite(address: UInt16, d: UInt8): State[NesState, Unit] =
    State.modify[Cartridge](_.chrWrite(address, d)).toNesState

  def getMirroring: State[NesState, Mirroring] =
    State.inspect(_.cartridge.mirroring)

  def fromString(program: String, offset: UInt16): Cartridge =
    program
      .sliding(2, 2)
      .map(Integer.parseInt(_, 16))
      .zipWithIndex
      .foldLeft(empty) { case (acc, (d, i)) =>
        acc.prgWrite(offset + i, d)
      }
      .prgWrite(0xFFFC, offset & 0xFF)
      .prgWrite(0xFFFD, (offset >> 8) & 0xFF)

  def nesFileDecoder: Decoder[Cartridge] = for {
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
        Decoder.point(Mapper000(prgRom, chrRom, prgRamSize, mirroring))
      else if (mapperId == 1)
        Decoder.point(Mapper001(prgRom, chrRom, prgRamSize))
      else
        Decoder.liftAttempt(Attempt.failure(Err(s"Unsupported mapper $mapperId!")))
  } yield cartridge

  def fromFile[F[_] : Sync : ContextShift](file: Path): F[Cartridge] = {
    Stream.resource(Blocker[F]).flatMap { blocker =>
      io.file.readAll[F](file, blocker, 4096).through(StreamDecoder.once(nesFileDecoder).toPipeByte)
    }.compile.toList.map(_.head)
  }
}