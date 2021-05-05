package scalanes

trait Cartridge {
  def prgRead(address: UInt16): UInt8
  def chrRead(address: UInt16): UInt8
  def prgWrite(address: UInt16, d: UInt8): Cartridge
  def chrWrite(address: UInt16, d: UInt8): Cartridge
  def reset: Cartridge
}

object Cartridge {

  implicit class CartridgeOps[A](val a: State[Cartridge, A]) extends AnyVal {
    def toNesState: State[NesState, A] = a.transformS(
      nes => nes.cartridge,
      (nesState, cartridge) => NesState.cartridge.set(cartridge)(nesState)
    )
  }

  val cpuReadLookup: Array[State[NesState, UInt8]] =
    (0x6000 to 0xffff).map(address => State.inspect[NesState, UInt8](_.cartridge.prgRead(address))).toArray

  val ppuReadLookup: Array[State[NesState, UInt8]] =
    (0x0000 to 0x1fff).map(address => State.inspect[NesState, UInt8](_.cartridge.chrRead(address))).toArray

  def cpuRead(address: UInt16): State[NesState, UInt8] =
    cpuReadLookup(address - 0x6000)

  def cpuWrite(address: UInt16, d: UInt8): NesState => NesState =
    State.modify[Cartridge](_.prgWrite(address, d)).toNesState.runS

  def ppuRead(address: UInt16): State[NesState, UInt8] =
    ppuReadLookup(address)

  def ppuWrite(address: UInt16, d: UInt8): NesState => NesState =
    State.modify[Cartridge](_.chrWrite(address, d)).toNesState.runS

  def reset: NesState => NesState =
    State.modify[Cartridge](_.reset).toNesState.runS

  def fromString(initial: Cartridge, program: String, offset: UInt16): Cartridge =
    program
      .sliding(2, 2)
      .map(Integer.parseInt(_, 16))
      .zipWithIndex
      .foldLeft(initial) { case (acc, (d, i)) =>
        acc.prgWrite(offset + i, d)
      }
      .prgWrite(0xfffc, offset & 0xff)
      .prgWrite(0xfffd, (offset >> 8) & 0xff)

}
