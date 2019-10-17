package scalanes

trait Mapper {
  def prgBanks: Int
  def chrBanks: Int
  def mapCpuAddress(address: UInt16): UInt16
  def mapPpuAddress(address: UInt16): UInt16
}

case class Mapper000(prgBanks: Int, chrBanks: Int) extends Mapper {
  override def mapCpuAddress(address: UInt16): UInt16 = {
    require(address >= 0x8000 && address <= 0xFFFF)
    address & (if (prgBanks > 1) 0x7FFF else 0x3FFF)
  }

  override def mapPpuAddress(address: UInt16): UInt16 = {
    require(address >= 0x0000 && address <= 0x1FFF)
    address
  }
}