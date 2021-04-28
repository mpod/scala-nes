package scalanes

import scala.annotation.tailrec

case class BankMap(offset: Int, sizeInKB: Int) {
  def nextBankMapOffset: Int = offset + sizeInKB * 1024
  def sizeInB: Int           = sizeInKB * 1024
}

object BankMap {
  def map32kB(bank: Int): BankMap =
    BankMap(bank * 32 * 1024, 32)

  def map16kB(bank: Int): BankMap =
    BankMap(bank * 16 * 1024, 16)

  def map8kB(bank: Int): BankMap =
    BankMap(bank * 8 * 1024, 8)

  def map4kB(bank: Int): BankMap =
    BankMap(bank * 4 * 1024, 4)
}

trait Mapper extends Cartridge {
  type Self <: Mapper

  def prgRom: Array[UInt8]
  def chrRom: Array[UInt8]
  def prgRam: Array[UInt8]
  def prgBankMaps: Array[BankMap]
  def chrBankMaps: Array[BankMap]

  protected def mapAddress(address: UInt16, bankMaps: Array[BankMap]): UInt16 = {
    @tailrec
    def helper(addr: UInt16, bankMaps: Array[BankMap], i: Int): UInt16 = {
      val bankMap = bankMaps(i)
      if (addr >= bankMap.nextBankMapOffset)
        helper(addr - bankMap.sizeInB, bankMaps, i + 1)
      else
        bankMap.offset + addr
    }
    helper(address, bankMaps, 0)
  }

  override def prgRead(address: UInt16): UInt8 =
    if (address >= 0x8000)
      prgRom(mapAddress(address - 0x8000, prgBankMaps))
    else
      prgRam(address - 0x6000)

  override def chrRead(address: UInt16): UInt8 =
    chrRom(mapAddress(address, chrBankMaps))

  override def prgWrite(address: UInt16, d: UInt8): Self

  override def chrWrite(address: UInt16, d: UInt8): Self

  override def reset: Self
}
