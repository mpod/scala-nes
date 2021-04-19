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
  def prgBankMaps: List[BankMap]
  def chrBankMaps: List[BankMap]

  protected def mapAddress(address: UInt16, bankMaps: List[BankMap]): UInt16 = {
    @tailrec
    def helper(addr: UInt16, bankMaps: List[BankMap]): UInt16 = bankMaps match {
      case bankMap :: tail if addr >= bankMap.nextBankMapOffset =>
        helper(addr - bankMap.sizeInB, tail)
      case bankMap :: _ =>
        bankMap.offset + addr
      case Nil =>
        throw new RuntimeException(f"Invalid cartridge address $address%#04x")
    }
    helper(address, bankMaps)
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
