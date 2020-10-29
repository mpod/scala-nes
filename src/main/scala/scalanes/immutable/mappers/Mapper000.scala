package scalanes.immutable.mappers

import scalanes.immutable._
import scalanes.immutable.{BankMap, Mapper}

case class Mapper000(prgRom: Vector[UInt8],
                     chrRom: Vector[UInt8],
                     prgRam: Vector[UInt8],
                     prgBankMaps: List[BankMap],
                     chrBankMaps: List[BankMap]
) extends Mapper {

  type SpecificMapper = Mapper000

  override def prgWrite(address: UInt16, d: UInt8): Mapper000 =
    if (address >= 0x8000) this
    else copy(prgRam.updated(address - 0x6000, d))

  override def chrWrite(address: UInt16, d: UInt8): Mapper000 = this

  override def reset: Mapper000 = this
}

object Mapper000 {

  def apply(prgRom: Vector[UInt8], chrRom: Vector[UInt8], prgRamSize: Int): Mapper000 = {
    val prgRomBanks =
      if (prgRom.size > 16 * 1024)
        List(BankMap(0, 16), BankMap(16 * 1024, 16))
      else
        List(BankMap(0, 16), BankMap(0, 16))
    new Mapper000(prgRom, chrRom, Vector.fill(prgRamSize)(0x00), prgRomBanks, List(BankMap(0, 8)))
  }

}
