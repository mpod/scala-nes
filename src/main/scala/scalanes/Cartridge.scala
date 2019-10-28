package scalanes

import cats.data.State
import scalanes.mappers.Mapper000

import scala.language.higherKinds

object Cartridge {

  implicit class CartridgeOps[A](val a: State[Cartridge, A]) extends AnyVal {
    def toNesState: State[NesState, A] = a.transformS(
      NesState.cartridge.get,
      (nesState, cartridge) => NesState.cartridge.set(cartridge)(nesState)
    )
  }

  def initial: Cartridge =
    Mapper000(Vector.fill(32 * 1024)(0x00), Vector.fill(8 * 1024)(0x00), 0)

  def cpuRead(address: UInt16): State[NesState, UInt8] =
    State.inspect(_.cartridge.prgRead(address))

  def cpuWrite(address: UInt16, d: UInt8): State[NesState, Unit] =
    State.modify[Cartridge](_.prgWrite(address, d)).toNesState

  def ppuRead(address: UInt16): State[NesState, UInt8] =
    State.inspect(_.cartridge.chrRead(address))

  def ppuWrite(address: UInt16, d: UInt8): State[NesState, Unit] =
    State.modify[Cartridge](_.chrWrite(address, d)).toNesState

  def reset: State[NesState, Unit] =
    State.modify[Cartridge](_.reset).toNesState

  def fromString(program: String, offset: UInt16): Cartridge =
    program
      .sliding(2, 2)
      .map(Integer.parseInt(_, 16))
      .zipWithIndex
      .foldLeft(initial) { case (acc, (d, i)) =>
        acc.prgWrite(offset + i, d)
      }
      .prgWrite(0xFFFC, offset & 0xFF)
      .prgWrite(0xFFFD, (offset >> 8) & 0xFF)

}