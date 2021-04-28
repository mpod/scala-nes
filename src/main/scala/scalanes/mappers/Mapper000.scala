package scalanes.mappers

import scalanes.{BankMap, Mapper, UInt16, UInt8}

class Mapper000(
  override val prgRom: Array[UInt8],
  override val chrRom: Array[UInt8],
  prgRamSize: Int
) extends Mapper {

  override type Self = this.type

  override val prgRam: Array[UInt8] = Array.fill(prgRamSize)(0x00)

  override val prgBankMaps: Array[BankMap] =
    if (prgRom.length > 16 * 1024)
      Array(BankMap(0, 16), BankMap(16 * 1024, 16))
    else
      Array(BankMap(0, 16), BankMap(0, 16))

  override val chrBankMaps: Array[BankMap] = Array(BankMap(0, 8))

  override def prgWrite(address: UInt16, d: UInt8): Self = {
    if (address < 0x8000) prgRam.update(address - 0x6000, d)
    this
  }

  override def chrWrite(address: UInt16, d: UInt8): Self = this

  override def reset: Self = this
}

object Mapper000 {}
