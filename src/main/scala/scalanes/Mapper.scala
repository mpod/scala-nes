package scalanes

case class BankMap(offset: Int, sizeInKB: Int)

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

trait Mapper {
  type SpecificMapper <: Mapper

  def prgRom: Vector[UInt8]
  def chrRom: Vector[UInt8]
  def prgRam: Vector[UInt8]
  def prgBankMaps: List[BankMap]
  def chrBankMaps: List[BankMap]

  protected def mapAddress(address: UInt16, bankMaps: List[BankMap]): UInt16 = {
    val (_, result) = bankMaps.foldLeft((address, Option.empty[UInt8])) {
      case ((addr, res), bankMap) if res.isEmpty =>
        if (addr >= (bankMap.offset + bankMap.sizeInKB * 1024))
          (addr - bankMap.sizeInKB * 1024, None)
        else
          (addr, Option(bankMap.offset + addr))
      case (acc, _) => acc
    }
    require(result.nonEmpty, s"$address")
    result.get
  }

  def prgRead(address: UInt16): UInt8 =
    if (address >= 0x8000)
      prgRom(mapAddress(address - 0x8000, prgBankMaps))
    else
      prgRam(address - 0x6000)

  def chrRead(address: UInt16): UInt8 =
    chrRom(mapAddress(address, chrBankMaps))

  def prgWrite(address: UInt16, d: UInt8): SpecificMapper

  def chrWrite(address: UInt16, d: UInt8): SpecificMapper

  def reset: SpecificMapper
}
