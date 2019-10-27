package scalanes.mappers

import scalanes.Mirroring.Mirroring
import scalanes._

case class Mapper000(prgRom: Vector[UInt8],
                     chrRom: Vector[UInt8],
                     prgRam: Vector[UInt8],
                     prgBankMaps: List[BankMap],
                     chrBankMaps: List[BankMap]
                    ) extends Mapper {

  type SpecificMapper = Mapper000

  override def prgWrite(address: UInt16, d: UInt8): Mapper000 = this

  override def chrWrite(address: UInt16, d: UInt8): Mapper000 = this

}

object Mapper000 {

  def apply(prgRom: Vector[UInt8], chrRom: Vector[UInt8], prgRamSize: Int): Mapper000 = {
    new Mapper000(prgRom, chrRom, Vector.fill(prgRamSize)(0x00), List(BankMap(0, 32)), List(BankMap(0, 8)))
  }

}
