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

  def cpuRead(address: UInt16): State[NesState, UInt8] =
    State.inspect(_.cartridge.prgRead(address))

  def cpuWrite(address: UInt16, d: UInt8): NesState => NesState =
    State.modify[Cartridge](_.prgWrite(address, d)).toNesState.runS

  def ppuRead(address: UInt16): State[NesState, UInt8] =
    State.inspect(_.cartridge.chrRead(address))

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
