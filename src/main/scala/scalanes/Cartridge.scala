package scalanes

import java.nio.file.Path

import cats.data.State
import monocle.Lens
import monocle.macros.GenLens

case class Cartridge(prgMem: Vector[UInt8], chrMem: Vector[UInt8], mapper: Mapper)

object Cartridge {
  val prgMem: Lens[Cartridge, Vector[UInt8]] = GenLens[Cartridge](_.prgMem)
  val chrMem: Lens[Cartridge, Vector[UInt8]] = GenLens[Cartridge](_.chrMem)
  val mapper: Lens[Cartridge, Mapper] = GenLens[Cartridge](_.mapper)

  def empty: Cartridge = Cartridge(Vector.fill(32 * 1024)(0x00), Vector.fill(8 * 1024)(0x00), Mapper000(2, 1))

  def cpuRead(address: UInt16): State[Cartridge, UInt8] =
    State.inspect { cartridge =>
      val mapped = cartridge.mapper.mapCpuAddress(address)
      cartridge.prgMem(mapped)
    }

  def cpuWrite(address: UInt16, d: UInt8): State[Cartridge, Unit] =
    State.modify { cartridge =>
      val mapped = cartridge.mapper.mapCpuAddress(address)
      prgMem.modify(_.updated(mapped, d))(cartridge)
    }

  def chrRead(address: UInt16): State[Cartridge, UInt8] =
    State.inspect { cartridge =>
      val mapped = cartridge.mapper.mapPpuAddress(address)
      cartridge.chrMem(mapped)
    }

  def chrWrite(address: UInt16, d: UInt8): State[Cartridge, Unit] =
    State.modify { cartridge =>
      val mapped = cartridge.mapper.mapPpuAddress(address)
      chrMem.modify(_.updated(mapped, d))(cartridge)
    }

  def fromString(program: String, offset: UInt16): Cartridge = {
    val s = empty
    val mapper = s.mapper.mapCpuAddress _
    val updated = program
      .sliding(2, 2)
      .map(Integer.parseInt(_, 16))
      .zipWithIndex
      .foldLeft(s.prgMem) { case (acc, (d, i)) =>
        acc.updated(mapper(offset + i), d)
      }
      .updated(mapper(0xFFFC), offset & 0xFF)
      .updated(mapper(0xFFFD), (offset >> 8) & 0xFF)
    prgMem.set(updated)(s)
  }

  def fromFile(file: Path): Cartridge = ???
}