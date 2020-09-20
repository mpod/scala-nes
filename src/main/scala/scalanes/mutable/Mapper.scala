package scalanes.mutable

case class BankMap(offset: Int, sizeInKB: Int) {
  def nextBankMapOffset: Int = offset + sizeInKB * 1024
  def sizeInB: Int = sizeInKB * 1024
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

  def prgRom: Vector[UInt8]
  def chrRom: Vector[UInt8]
  def prgRam: Array[UInt8]
  def prgBankMaps: List[BankMap]
  def chrBankMaps: List[BankMap]

  protected def mapAddress(address: UInt16, bankMaps: List[BankMap]): UInt16 = {
    val (_, result) =
      bankMaps.foldLeft((address, Option.empty[UInt8])) {
        case ((addr, None), bankMap) if addr >= bankMap.nextBankMapOffset =>
          (addr - bankMap.sizeInB, None)
        case ((addr, None), bankMap) =>
          (addr, Option(bankMap.offset + addr))
        case (acc, _) =>
          acc
      }
    require(result.nonEmpty, s"$address")
    result.get
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
