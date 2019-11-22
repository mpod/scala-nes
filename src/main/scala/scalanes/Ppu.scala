package scalanes

import cats.implicits._
import monocle.Lens
import monocle.function.Index._
import monocle.macros.GenLens
import scalanes.AddressIncrementMode.AddressIncrementMode
import scalanes.BackgroundTableAddress.BackgroundTableAddress
import scalanes.MasterSlaveMode.MasterSlaveMode
import scalanes.Mirroring.Mirroring
import scalanes.NametableAddress.NametableAddress
import scalanes.NmiMode.NmiMode
import scalanes.SpritePriority.SpritePriority
import scalanes.SpriteSize.SpriteSize
import scalanes.SpriteTableAddress.SpriteTableAddress

import scala.language.implicitConversions

case class PpuState(nametables: Vector[UInt8],
                    palettes: Vector[UInt8],
                    registers: PpuRegisters,
                    mirroring: Mirroring,
                    bgRenderingState: BgRenderingState,
                    spritesState: SpritesState,
                    pixels: Vector[Rgb]) {

  def reset: PpuState =
    copy(registers = registers.reset, bgRenderingState = bgRenderingState.reset)

}

object PpuState {
  val nametables: Lens[PpuState, Vector[UInt8]]          = GenLens[PpuState](_.nametables)
  val palettes: Lens[PpuState, Vector[UInt8]]            = GenLens[PpuState](_.palettes)
  val registers: Lens[PpuState, PpuRegisters]            = GenLens[PpuState](_.registers)
  val mirroring: Lens[PpuState, Mirroring]               = GenLens[PpuState](_.mirroring)
  val bgRenderingState: Lens[PpuState, BgRenderingState] = GenLens[PpuState](_.bgRenderingState)
  val spritesState: Lens[PpuState, SpritesState]         = GenLens[PpuState](_.spritesState)
  val pixels: Lens[PpuState, Vector[Rgb]]                = GenLens[PpuState](_.pixels)

  def initial(mirroring: Mirroring): PpuState = PpuState(
    Vector.fill(2 * 1024)(0x00),
    Vector.fill(32)(0x00),
    PpuRegisters.initial,
    mirroring,
    BgRenderingState.initial,
    SpritesState.initial,
    Vector.fill(240 * 256)(Rgb.initial)
  )
}

case class PpuRegisters(ctrl: PpuCtrl, mask: PpuMask, status: PpuStatus, data: UInt8, loopy: LoopyRegisters) {
  def reset: PpuRegisters = PpuRegisters.initial
}

object PpuRegisters {
  val ctrl: Lens[PpuRegisters, PpuCtrl]         = GenLens[PpuRegisters](_.ctrl)
  val mask: Lens[PpuRegisters, PpuMask]         = GenLens[PpuRegisters](_.mask)
  val status: Lens[PpuRegisters, PpuStatus]     = GenLens[PpuRegisters](_.status)
  val data: Lens[PpuRegisters, UInt8]           = GenLens[PpuRegisters](_.data)
  val loopy: Lens[PpuRegisters, LoopyRegisters] = GenLens[PpuRegisters](_.loopy)

  def initial: PpuRegisters =
    PpuRegisters(PpuCtrl.initial, PpuMask.initial, PpuStatus.initial, 0x00, LoopyRegisters.initial)
}

case class PpuCtrl(nametable: NametableAddress,
                   incrementMode: AddressIncrementMode,
                   spriteTableAddress: SpriteTableAddress,
                   backgroundTableAddress: BackgroundTableAddress,
                   spriteSize: SpriteSize,
                   masterSlaveMode: MasterSlaveMode,
                   nmiMode: NmiMode) {

  def toUInt8: UInt8 =
    ((nametable.id              & 0x03) << 0) |
    ((incrementMode.id          & 0x01) << 2) |
    ((spriteTableAddress.id     & 0x01) << 3) |
    ((backgroundTableAddress.id & 0x01) << 4) |
    ((spriteSize.id             & 0x01) << 5) |
    ((masterSlaveMode.id        & 0x01) << 6) |
    ((nmiMode.id                & 0x01) << 7)

}

object PpuCtrl {
  def apply(a: UInt8): PpuCtrl = new PpuCtrl(
    NametableAddress(a & 0x03),
    AddressIncrementMode((a >> 2) & 0x01),
    SpriteTableAddress((a >> 3) & 0x01),
    BackgroundTableAddress((a >> 4) & 0x01),
    SpriteSize((a >> 5) & 0x01),
    MasterSlaveMode((a >> 6) & 0x01),
    NmiMode((a >> 7) & 0x01)
  )

  def initial: PpuCtrl = apply(0)
}

object NametableAddress extends Enumeration {
  type NametableAddress = Val
  protected case class Val(address: UInt16) extends super.Val
  implicit def valueToVal(x: Value): Val = x.asInstanceOf[Val]
  val First  = Val(0x2000)
  val Second = Val(0x2400)
  val Third  = Val(0x2800)
  val Fourth = Val(0x2C00)
}

object AddressIncrementMode extends Enumeration {
  type AddressIncrementMode = Val
  protected case class Val(delta: Int) extends super.Val
  implicit def valueToVal(x: Value): Val = x.asInstanceOf[Val]
  val Add1  = Val(1)
  val Add32 = Val(32)
}

object SpriteTableAddress extends Enumeration {
  type SpriteTableAddress = Val
  protected case class Val(address: UInt16) extends super.Val
  implicit def valueToVal(x: Value): Val = x.asInstanceOf[Val]
  val First  = Val(0x0000)
  val Second = Val(0x1000)
}

object BackgroundTableAddress extends Enumeration {
  type BackgroundTableAddress = Val
  protected case class Val(address: UInt16) extends super.Val
  implicit def valueToVal(x: Value): Val = x.asInstanceOf[Val]
  val First  = Val(0x0000)
  val Second = Val(0x1000)
}

object SpriteSize extends Enumeration {
  type SpriteSize = Val
  protected case class Val(height: Int) extends super.Val
  implicit def valueToVal(x: Value): Val = x.asInstanceOf[Val]
  val Small = Val(8)
  val Large = Val(16)
}

object MasterSlaveMode extends Enumeration {
  type MasterSlaveMode = Value
  val ReadBackdropFromExtPins, OutputColorOnExtPins = Value
}

object NmiMode extends Enumeration {
  type NmiMode = Value
  val Off, On = Value
}

case class PpuMask(greyscale: Boolean,
                   renderBackgroundLeft: Boolean,
                   renderSpritesLeft: Boolean,
                   renderBackground: Boolean,
                   renderSprites: Boolean,
                   enhanceRed: Boolean,
                   enhanceGreen: Boolean,
                   enhanceBlue: Boolean) {

  def asUInt8: UInt8 =
    (if (greyscale)            0x01 << 0 else 0x00) |
    (if (renderBackgroundLeft) 0x01 << 1 else 0x00) |
    (if (renderSpritesLeft)    0x01 << 2 else 0x00) |
    (if (renderBackground)     0x01 << 3 else 0x00) |
    (if (renderSprites)        0x01 << 4 else 0x00) |
    (if (enhanceRed)           0x01 << 5 else 0x00) |
    (if (enhanceGreen)         0x01 << 6 else 0x00) |
    (if (enhanceBlue)          0x01 << 7 else 0x00)

}

object PpuMask {
  def apply(d: UInt8): PpuMask = {
    require((d & 0xFF) == d)
    new PpuMask(
      (d >> 0) & 0x01,
      (d >> 1) & 0x01,
      (d >> 2) & 0x01,
      (d >> 3) & 0x01,
      (d >> 4) & 0x01,
      (d >> 5) & 0x01,
      (d >> 6) & 0x01,
      (d >> 7) & 0x01
    )
  }

  def initial: PpuMask = apply(0)
}

case class PpuStatus(spriteOverflow: Boolean, spriteZeroHit: Boolean, verticalBlank: Boolean) {

  def asUInt8: UInt8 =
    (if (spriteOverflow) 0x01 << 5 else 0x00) |
    (if (spriteZeroHit)  0x01 << 6 else 0x00) |
    (if (verticalBlank)  0x01 << 7 else 0x00)

}

object PpuStatus {
  val spriteOverflow: Lens[PpuStatus, Boolean] = GenLens[PpuStatus](_.spriteOverflow)
  val spriteZeroHit: Lens[PpuStatus, Boolean]  = GenLens[PpuStatus](_.spriteZeroHit)
  val verticalBlank: Lens[PpuStatus, Boolean]  = GenLens[PpuStatus](_.verticalBlank)

  def apply(a: UInt8): PpuStatus = new PpuStatus(a & 0x20, a & 0x40, a & 0x80)

  def initial: PpuStatus = apply(0)
}

case class LoopyRegisters(v: LoopyAddress, t: LoopyAddress, x: UInt3, w: Boolean) {
  require((x & 0x7) == x)

  def reset: LoopyRegisters = LoopyRegisters.initial
}

object LoopyRegisters {
  val v: Lens[LoopyRegisters, LoopyAddress] = GenLens[LoopyRegisters](_.v)
  val t: Lens[LoopyRegisters, LoopyAddress] = GenLens[LoopyRegisters](_.t)
  val x: Lens[LoopyRegisters, UInt3]        = GenLens[LoopyRegisters](_.x)
  val w: Lens[LoopyRegisters, Boolean]      = GenLens[LoopyRegisters](_.w)

  def initial: LoopyRegisters = LoopyRegisters(LoopyAddress.initial, LoopyAddress.initial, 0, w = false)
}

case class LoopyAddress(coarseX: UInt5, coarseY: UInt5, nametableX: UInt1, nametableY: UInt1, fineY: UInt3) {
  require((coarseX & 0x1F) == coarseX)
  require((coarseY & 0x1F) == coarseY)
  require((nametableX & 0x1) == nametableX)
  require((nametableY & 0x1) == nametableY)
  require((fineY & 0x7) == fineY)

  def setCoarseX(d: UInt5): LoopyAddress    = LoopyAddress.coarseX.set(d)(this)
  def setCoarseY(d: UInt5): LoopyAddress    = LoopyAddress.coarseY.set(d)(this)
  def setNametables(d: UInt2): LoopyAddress = (LoopyAddress.nametableX.set(d & 0x01) andThen
                                               LoopyAddress.nametableY.set((d >> 1) & 0x01))(this)
  def nametables: UInt2                     = (nametableY << 1) | nametableX
  def setFineY(d: UInt3): LoopyAddress      = LoopyAddress.fineY.set(d)(this)
  def setNametableX(d: UInt1): LoopyAddress = LoopyAddress.nametableX.set(d)(this)
  def setNametableY(d: UInt1): LoopyAddress = LoopyAddress.nametableY.set(d)(this)
  def flipNametableX(): LoopyAddress        = LoopyAddress.nametableX.modify(_ ^ 0x1)(this)
  def flipNametableY(): LoopyAddress        = LoopyAddress.nametableY.modify(_ ^ 0x1)(this)

  def asUInt16: UInt16 = (fineY << 12) | (nametableY << 11) | (nametableX << 10) | (coarseY << 5) | coarseX
}

object LoopyAddress {
  val coarseX: Lens[LoopyAddress, UInt5]    = GenLens[LoopyAddress](_.coarseX)
  val coarseY: Lens[LoopyAddress, UInt5]    = GenLens[LoopyAddress](_.coarseY)
  val nametableX: Lens[LoopyAddress, UInt1] = GenLens[LoopyAddress](_.nametableX)
  val nametableY: Lens[LoopyAddress, UInt1] = GenLens[LoopyAddress](_.nametableY)
  val fineY: Lens[LoopyAddress, UInt3]      = GenLens[LoopyAddress](_.fineY)

  def apply(address: UInt16): LoopyAddress = new LoopyAddress(
    address & 0x1F,
    (address >> 5) & 0x1F,
    (address >> 10) & 0x1,
    (address >> 11) & 0x1,
    (address >> 12) & 0x7
  )

  def initial: LoopyAddress = LoopyAddress(0)
}

case class BgRenderingState(tileLo: UInt8,
                            tileHi: UInt8,
                            tileAttr: UInt8,
                            nextTileLo: UInt8,
                            nextTileHi: UInt8,
                            nextTileAttr: UInt8) {

  require((tileLo & 0xFF) == tileLo)
  require((tileHi & 0xFF) == tileHi)
  require((tileAttr & 0xFF) == tileAttr)
  require((nextTileLo & 0xFF) == nextTileLo)
  require((nextTileHi & 0xFF) == nextTileHi)
  require((nextTileAttr & 0xFF) == nextTileAttr)

  def loadRegisters: BgRenderingState = {
    copy(
      tileLo   = nextTileLo,
      tileHi   = nextTileHi,
      tileAttr = nextTileAttr
    )
  }

  def setNextTileAttr(d: UInt8): BgRenderingState = copy(nextTileAttr = d)

  def setNextTileLo(d: UInt8): BgRenderingState = copy(nextTileLo = d)

  def setNextTileHi(d: UInt8): BgRenderingState = copy(nextTileHi = d)

  def reset: BgRenderingState = BgRenderingState.initial
}

object BgRenderingState {
  def initial: BgRenderingState = BgRenderingState(0, 0, 0, 0, 0, 0)
}

object Mirroring extends Enumeration {
  type Mirroring = Value
  val Vertical, Horizontal, OneScreenLowerBank, OneScreenUpperBank = Value
}

case class Rgb(r: Int, g: Int, b: Int)

object Rgb {
  val palette: Vector[Rgb] = Vector(
    Rgb(84, 84, 84),    Rgb(0, 30, 116),    Rgb(8, 16, 144),    Rgb(48, 0, 136),
    Rgb(68, 0, 100),    Rgb(92, 0, 48),     Rgb(84, 4, 0),      Rgb(60, 24, 0),
    Rgb(32, 42, 0),     Rgb(8, 58, 0),      Rgb(0, 64, 0),      Rgb(0, 60, 0),
    Rgb(0, 50, 60),     Rgb(0, 0, 0),       Rgb(0, 0, 0),       Rgb(0, 0, 0),

    Rgb(152, 150, 152), Rgb(8, 76, 196),    Rgb(48, 50, 236),   Rgb(92, 30, 228),
    Rgb(136, 20, 176),  Rgb(160, 20, 100),  Rgb(152, 34, 32),   Rgb(120, 60, 0),
    Rgb(84, 90, 0),     Rgb(40, 114, 0),    Rgb(8, 124, 0),     Rgb(0, 118, 40),
    Rgb(0, 102, 120),   Rgb(0, 0, 0),       Rgb(0, 0, 0),       Rgb(0, 0, 0),

    Rgb(236, 238, 236), Rgb(76, 154, 236),  Rgb(120, 124, 236), Rgb(176, 98, 236),
    Rgb(228, 84, 236),  Rgb(236, 88, 180),  Rgb(236, 106, 100), Rgb(212, 136, 32),
    Rgb(160, 170, 0),   Rgb(116, 196, 0),   Rgb(76, 208, 32),   Rgb(56, 204, 108),
    Rgb(56, 180, 204),  Rgb(60, 60, 60),    Rgb(0, 0, 0),       Rgb(0, 0, 0),

    Rgb(236, 238, 236), Rgb(168, 204, 236), Rgb(188, 188, 236), Rgb(212, 178, 236),
    Rgb(236, 174, 236), Rgb(236, 174, 212), Rgb(236, 180, 176), Rgb(228, 196, 144),
    Rgb(204, 210, 120), Rgb(180, 222, 120), Rgb(168, 226, 144), Rgb(152, 226, 180),
    Rgb(160, 214, 228), Rgb(160, 162, 160), Rgb(0, 0, 0),       Rgb(0, 0, 0)
  )

  val initial: Rgb = Rgb(0, 0, 0)
}

case class OamEntry(y: UInt8, id: UInt8, attribute: UInt8, x: UInt8) {
  require((y & 0xFF) == y)
  require((id & 0xFF) == id)
  require((attribute & 0xFF) == attribute)
  require((x & 0xFF) == x)

  def readField(i: Int): UInt8 = i match {
    case 0 => y
    case 1 => id
    case 2 => attribute
    case 3 => x
    case _ => throw new IllegalArgumentException("Index should be in range 0..3")
  }

  def writeField(i: Int, d: UInt8): OamEntry = i match {
    case 0 => copy(y = d)
    case 1 => copy(id = d)
    case 2 => copy(attribute = d)
    case 3 => copy(x = d)
    case _ => throw new IllegalArgumentException("Index should be in range 0..3")
  }

  val palette: UInt3            = (attribute & 0x03) + 4
  val priority: SpritePriority  = SpritePriority(attribute & 0x10)
  val flipHorizontally: Boolean = attribute & 0x40
  val flipVertically: Boolean   = attribute & 0x80
}

object OamEntry {
  val y: Lens[OamEntry, UInt8] = GenLens[OamEntry](_.y)
  val x: Lens[OamEntry, UInt8] = GenLens[OamEntry](_.x)
  val initial: OamEntry        = OamEntry(0x00, 0x00, 0x00, 0x00)
}

case class ScanlineOamEntry(sprite: OamEntry, spriteLo: UInt8, spriteHi: UInt8, isSpriteZero: Boolean) {
  require((spriteLo & 0xFF) == spriteLo)
  require((spriteHi & 0xFF) == spriteHi)

  def shiftRegisters: ScanlineOamEntry =
    copy(spriteLo = (spriteLo << 1) & 0xFF, spriteHi = (spriteHi << 1) & 0xFF)

}

object ScanlineOamEntry {
  val sprite: Lens[ScanlineOamEntry, OamEntry] = GenLens[ScanlineOamEntry](_.sprite)
  val spriteLo: Lens[ScanlineOamEntry, UInt8]  = GenLens[ScanlineOamEntry](_.spriteLo)
  val spriteHi: Lens[ScanlineOamEntry, UInt8]  = GenLens[ScanlineOamEntry](_.spriteHi)

  def apply(sprite: OamEntry, isZeroSprite: Boolean): ScanlineOamEntry =
    new ScanlineOamEntry(sprite, 0x00, 0x00, isZeroSprite)
}

object SpritePriority extends Enumeration {
  type SpritePriority = Value
  val InFrontOfBackground, BehindBackground = Value
}

case class SpritesState(oam: Vector[OamEntry], oamAddress: UInt8, scanlineOam: Vector[ScanlineOamEntry])

object SpritesState {
  val oam: Lens[SpritesState, Vector[OamEntry]]                 = GenLens[SpritesState](_.oam)
  val oamAddress: Lens[SpritesState, UInt8]                     = GenLens[SpritesState](_.oamAddress)
  val scanlineOam: Lens[SpritesState, Vector[ScanlineOamEntry]] = GenLens[SpritesState](_.scanlineOam)

  val initial: SpritesState = SpritesState(Vector.fill(64)(OamEntry.initial), 0x00, Vector.empty)
}

object Ppu {

  private val ctrlRegister   = PpuState.registers composeLens PpuRegisters.ctrl
  private val statusRegister = PpuState.registers composeLens PpuRegisters.status
  private val maskRegister   = PpuState.registers composeLens PpuRegisters.mask
  private val loopyRegisters = PpuState.registers composeLens PpuRegisters.loopy

  implicit class PpuStateOps[A](val a: State[PpuState, A]) extends AnyVal {
    def toNesState: State[NesState, A] = a.transformS(
      NesState.ppuState.get,
      (nesState, ppuState) => NesState.ppuState.set(ppuState)(nesState)
    )
  }

  def setVerticalBlankS(d: Boolean): State[NesState, NesState] = State { nes =>
    val updated = (NesState.ppuState composeLens statusRegister composeLens PpuStatus.verticalBlank).set(d)(nes)
    (updated, updated)
  }

  def setVerticalBlank(d: Boolean)(ppu: PpuState): PpuState =
    (statusRegister composeLens PpuStatus.verticalBlank).set(d)(ppu)

  def setSpriteZeroHit(ppu: PpuState): PpuState =
    (statusRegister composeLens PpuStatus.spriteZeroHit).set(true)(ppu)

  def clearSpriteZeroHit(ppu: PpuState): PpuState =
    (statusRegister composeLens PpuStatus.spriteZeroHit).set(false)(ppu)

  def setSpriteOverflow(d: Boolean)(ppu: PpuState): PpuState =
    (statusRegister composeLens PpuStatus.spriteOverflow).set(d)(ppu)

  def clearScanlineOam(ppu: PpuState): PpuState =
    (PpuState.spritesState composeLens SpritesState.scanlineOam).set(Vector.empty)(ppu)

  def setData(d: UInt8)(ppu: PpuState): PpuState =
    (PpuState.registers composeLens PpuRegisters.data).set(d)(ppu)

  def setCtrl(d: UInt8)(ppu: PpuState): PpuState =
    ctrlRegister.set(PpuCtrl(d))(ppu)

  def clearLoopyW(ppu: PpuState): PpuState =
    (loopyRegisters composeLens LoopyRegisters.w).set(false)(ppu)

  def getLoopyV(ppu: PpuState): LoopyAddress = ppu.registers.loopy.v

  def setLoopyV(d: LoopyAddress)(ppu: PpuState): PpuState =
    (loopyRegisters composeLens LoopyRegisters.v).set(d)(ppu)

  def getLoopyT(ppu: PpuState): LoopyAddress = ppu.registers.loopy.t

  def setLoopyT(d: LoopyAddress)(ppu: PpuState): PpuState =
    (loopyRegisters composeLens LoopyRegisters.t).set(d)(ppu)

  def setLoopyX(d: UInt3)(ppu: PpuState): PpuState =
    (loopyRegisters composeLens LoopyRegisters.x).set(d)(ppu)

  def setLoopyW(d: Boolean)(ppu: PpuState): PpuState =
    (loopyRegisters composeLens LoopyRegisters.w).set(d)(ppu)

  def isRendering(ppu: PpuState): Boolean =
    ppu.registers.mask.renderBackground || ppu.registers.mask.renderSprites

  def isRenderingBackground(ppu: PpuState): Boolean =
    ppu.registers.mask.renderBackground

  def setBgRenderingState(d: BgRenderingState)(ppu: PpuState): PpuState =
    PpuState.bgRenderingState.set(d)(ppu)

  def modifyBgRenderingState(f: BgRenderingState => BgRenderingState)(ppu: PpuState): PpuState =
    PpuState.bgRenderingState.modify(f)(ppu)

  def incScrollX(ppu: PpuState): PpuState =
    if (isRendering(ppu)) {
      val v = getLoopyV(ppu)
      val updatedV = if (v.coarseX == 31) v.setCoarseX(0).flipNametableX()
      else v.setCoarseX(v.coarseX + 1)
      setLoopyV(updatedV)(ppu)
    } else
      ppu

  def incScrollY(ppu: PpuState): PpuState =
    if (isRendering(ppu)) {
      val v = getLoopyV(ppu)
      val updatedV = if (v.fineY < 7) v.setFineY(v.fineY + 1)
      else if (v.coarseY == 29) v.setFineY(0).setCoarseY(0).flipNametableY()
      else if (v.coarseY == 31) v.setFineY(0).setCoarseY(0)
      else v.setFineY(0).setCoarseY(v.coarseY + 1)
      setLoopyV(updatedV)(ppu)
    } else
      ppu

  def transferAddressX(ppu: PpuState): PpuState =
    if (isRendering(ppu)) {
      val v = getLoopyV(ppu)
      val t = getLoopyT(ppu)
      val updatedV = v.setCoarseX(t.coarseX).setNametableX(t.nametableX)
      setLoopyV(updatedV)(ppu)
    } else
      ppu

  def transferAddressY(ppu: PpuState): PpuState =
    if (isRendering(ppu)) {
      val v = getLoopyV(ppu)
      val t = getLoopyT(ppu)
      val updatedV = v.setFineY(t.fineY).setNametableY(t.nametableY).setCoarseY(t.coarseY)
      setLoopyV(updatedV)(ppu)
    } else
      ppu

  private def mapToNametableIndex(address: UInt16, mirroring: Mirroring): UInt16 = {
    val addr = address & 0x0FFF
    if (mirroring == Mirroring.Vertical) addr & 0x07FF
    else if (mirroring == Mirroring.Horizontal) ((addr & 0x800) >> 1) | (addr & 0x3FF)
    else addr
  }

  def readNametables(address: UInt16)(ppu: PpuState): UInt8 = {
    require((address & 0xFFFF) == address)
    require(address >= 0x2000 && address < 0x3F00)

    val addr = mapToNametableIndex(address, ppu.mirroring)
    ppu.nametables(addr)
  }

  def writeNametables(address: UInt16, d: UInt8)(ppu: PpuState): PpuState = {
    require((address & 0xFFFF) == address)
    require(address >= 0x2000 && address < 0x3F00)

    val addr = mapToNametableIndex(address, ppu.mirroring)
    PpuState.nametables.modify(_.updated(addr, d))(ppu)
  }

  private def mapToPalettesIndex(address: UInt16): UInt16 = {
    val addr = address & 0x1F
    // Mirror $3F10, $3F14, $3F18, $3F1C to $3F00, $3F04, $3F08, $3F0C
    if ((addr & 0x13) == 0x10) addr & ~0x10 else addr
  }

  def getColor(palette: Int, pixel: Int)(ppu: PpuState): Rgb = {
    require(palette >= 0 && palette < 8)
    require(pixel >= 0 && pixel < 4)
    val colorAddress = 0x3F00 + (palette << 2) + pixel
    val colorValue = if (ppu.registers.mask.renderBackground)
      ppu.palettes(mapToPalettesIndex(colorAddress))
    else
      ppu.palettes(mapToPalettesIndex(0x0000))
    Rgb.palette(colorValue)
  }

  def readPalettes(address: UInt16)(ppu: PpuState): UInt8 = {
    require((address & 0xFFFF) == address)
    require(address >= 0x3F00 && address < 0x4000, s"failed: $address")

    val addr      = mapToPalettesIndex(address)
    val color     = ppu.palettes(addr)
    val greyscale = ppu.registers.mask.greyscale
    if (greyscale) color & 0x30
    else color
  }

  def writePalettes(address: UInt16, d: UInt8)(ppu: PpuState): PpuState = {
    require((address & 0xFFFF) == address)
    require(address >= 0x3F00 && address < 0x3FFF)

    val addr = mapToPalettesIndex(address)
    PpuState.palettes.modify(_.updated(addr, d))(ppu)
  }

  def readOam: State[NesState, UInt8] = State.inspect { nes =>
    val oamAddress = nes.ppuState.spritesState.oamAddress
    val entryIndex = oamAddress / 4
    val field      = oamAddress % 4
    nes.ppuState.spritesState.oam(entryIndex).readField(field)
  }

  def writeOam(oamAddress: UInt8, d: UInt8): State[NesState, Unit] = {
    val entryIndex = oamAddress / 4
    val field      = oamAddress % 4
    val update     = (PpuState.spritesState composeLens SpritesState.oam composeOptional index(entryIndex)).modify(
      _.writeField(field, d)
    )
    modifyNesState(update)
  }

  def evaluateSprites(scanline: Int)(ppu: PpuState): PpuState = {
    val spriteSize = ppu.registers.ctrl.spriteSize
    val affectedSprites = ppu
      .spritesState
      .oam
      .zipWithIndex
      .filter { case (e, _) =>
        val diff = scanline - e.y
        diff >= 0 && diff < spriteSize.height
      }
      .map { case (e, i) =>
        ScanlineOamEntry(e, i == 0)
      }
    val spriteOverflow = affectedSprites.size > 8
    val scanlineSprites = affectedSprites.take(8)

    val update =
      (PpuState.spritesState composeLens SpritesState.scanlineOam).set(scanlineSprites) andThen
      (statusRegister composeLens PpuStatus.spriteOverflow).set(spriteOverflow)

    update(ppu)
  }

  private def flipByte(d: UInt8): UInt8 = {
    val d1 = (d  & 0xF0) >> 4 | (d  & 0x0F) << 4
    val d2 = (d1 & 0xCC) >> 2 | (d1 & 0x33) << 2
    val d3 = (d2 & 0xAA) >> 1 | (d2 & 0x55) << 1
    d3
  }

  def loadSprites(scanline: Int): State[NesState, NesState] = State.get[NesState].flatMap { nes =>
    val ppu = nes.ppuState
    ppu.spritesState.scanlineOam.zipWithIndex.map { case (e, i) =>
      val spriteSize = ppu.registers.ctrl.spriteSize
      val addrBase =
        if (spriteSize == SpriteSize.Small)
          ppu.registers.ctrl.spriteTableAddress.address | (e.sprite.id << 4)
        else
          ((e.sprite.id & 0x01) << 12) | ((e.sprite.id & 0xFE) << 4)
      val row =
        if (e.sprite.flipVertically)
          spriteSize.height - 1 - (scanline - e.sprite.y) % spriteSize.height
        else
          (scanline - e.sprite.y) % spriteSize.height
      val addr = addrBase + row

      for {
        lo <- ppuRead(addr + 0)
        hi <- ppuRead(addr + 8)
        spriteLo = if (e.sprite.flipHorizontally) flipByte(lo) else lo
        spriteHi = if (e.sprite.flipHorizontally) flipByte(hi) else hi
        s <- State { nes: NesState =>
          val scanlineEntry = SpritesState.scanlineOam composeOptional index(i)
          val updated = (NesState.ppuState composeLens PpuState.spritesState).modify(
            (scanlineEntry composeLens ScanlineOamEntry.spriteLo).set(spriteLo) andThen
            (scanlineEntry composeLens ScanlineOamEntry.spriteHi).set(spriteHi)
          )(nes)
          (updated, updated)
        }
      } yield s
    }
    .foldLeft(State.get[NesState]) { case (acc, a) => acc >> a }
  }

  def cpuRead(address: UInt16): State[NesState, UInt8] = State.get.flatMap { nes =>
    // The PPU exposes eight memory-mapped registers to the CPU, which are mapped to the
    // address range $2000-$2007. Addresses between $2008 and $3FFF are mirrored.
    require(address >= 0x2000 && address <= 0x3FFF)
    val zero        = State.pure[NesState, UInt8](0x00)
    val ppu: PpuState = nes.ppuState
    address & 0x0007 match {
      case 0x0000 => // PPUCTRL
        zero

      case 0x0001 => // PPUMASK
        zero

      case 0x0002 => // PPUSTATUS
        val data   = ppu.registers.data
        val status = ppu.registers.status.asUInt8
        val update = clearLoopyW _ andThen setVerticalBlank(false)
        val d      = (status & 0xE0) | (data & 0x1F)
        State { nes =>
          (NesState.ppuState.modify(update)(nes), d)
        }

      case 0x0003 => // OAMADDR
        zero

      case 0x0004 => // OAMDATA
        readOam

      case 0x0005 => // PPUSCROLL
        zero

      case 0x0006 => // PPUADDR
        zero

      case _ =>      // PPUDATA
        val d1   = ppu.registers.data
        val v1   = ppu.registers.loopy.v
        val ctrl = ppu.registers.ctrl
        val v2   = LoopyAddress(v1.asUInt16 + ctrl.incrementMode.delta)
        ppuRead(v1.asUInt16).transform { (ns, d2) =>
          val update = setData(d2) _ andThen setLoopyV(v2)
          val d = if (v1.asUInt16 >= 0x3F00) d2 else d1
          (NesState.ppuState.modify(update)(ns), d)
        }

    }
  }

  def cpuWrite(address: UInt16, d: UInt8): State[NesState, Unit] = State.get.flatMap { nes =>
    require(address >= 0x2000 && address <= 0x3FFF)
    require((d & 0xFF) == d)
    val empty = State.set(nes)
    val ppu: PpuState = nes.ppuState
    val t1 = ppu.registers.loopy.t
    address & 0x0007 match {
      case 0x0000 => // PPUCTRL
        val ctrl   = PpuCtrl(d)
        val t2     = t1.setNametables(ctrl.nametable.id)
        val update = setCtrl(d) _ andThen setLoopyT(t2)
        modifyNesState(update)

      case 0x0001 => // PPUMASK
        val update = maskRegister.set(PpuMask(d))
        modifyNesState(update)

      case 0x0002 => // PPUSTATUS
        empty

      case 0x0003 => // OAMADDR
        val update = (PpuState.spritesState composeLens SpritesState.oamAddress).set(d)
        modifyNesState(update)

      case 0x0004 => // OAMDATA
        writeOam(ppu.spritesState.oamAddress, d)

      case 0x0005 => // PPUSCROLL
        val update = if (ppu.registers.loopy.w) {
          val t2 = t1.setCoarseY((d >> 3) & 0x1F).setFineY(d & 0x07)
          setLoopyT(t2) _ andThen setLoopyW(false)
        } else {
          val t2 = t1.setCoarseX((d >> 3) & 0x1F)
          setLoopyX(d & 0x07) _ andThen setLoopyT(t2) andThen setLoopyW(true)
        }
        modifyNesState(update)

      case 0x0006 => // PPUADDR
        val update = if (ppu.registers.loopy.w) {
          val t2 = LoopyAddress((t1.asUInt16 & 0xFF00) | d)
          setLoopyT(t2) _ andThen setLoopyV(t2) andThen setLoopyW(false)
        } else {
          val t2 = LoopyAddress(((d & 0x3F) << 8) | (t1.asUInt16 & 0x00FF) )
          setLoopyT(t2) _ andThen setLoopyW(true)
        }
        modifyNesState(update)

      case _ =>      // PPUDATA
        val ctrl = ppu.registers.ctrl
        val v1   = ppu.registers.loopy.v
        val v2   = LoopyAddress(v1.asUInt16 + ctrl.incrementMode.delta)
        ppuWrite(v1.asUInt16, d).modify(NesState.ppuState.modify(setLoopyV(v2)))

    }
  }

  def ppuRead(address: UInt16): State[NesState, UInt8] = State.get.flatMap { _ =>
    require((address & 0x3FFF) == address)

    if (address >= 0x0000 && address <= 0x1FFF)
      Cartridge.ppuRead(address)
    else if (address >= 0x2000 && address <= 0x3EFF)
      State.inspect(nes => readNametables(address)(nes.ppuState))
    else
      State.inspect(nes => readPalettes(address)(nes.ppuState))
  }

  def ppuWrite(address: UInt16, d: UInt8): State[NesState, Unit] = State.get.flatMap { _ =>
    require((address & 0x3FFF) == address)

    if (address >= 0x0000 && address <= 0x1FFF)
      Cartridge.ppuWrite(address, d)
    else if (address >= 0x2000 && address <= 0x3EFF)
      modifyNesState(writeNametables(address, d))
    else
      modifyNesState(writePalettes(address, d))
  }

  private def modifyNesState(f: PpuState => PpuState): State[NesState, Unit] =
    State.modify(NesState.ppuState.modify(f))

  private def pixel(x: Int,
                    tileHi: UInt8,
                    tileLo: UInt8,
                    tileAttr: UInt2,
                    ppu: PpuState): (Rgb, Boolean) = {

    val fineX = ppu.registers.loopy.x

    val (bgPixel, bgPalette) =
      if (ppu.registers.mask.renderBackground) {
        val bitSel  = 0x80 >> ((x + fineX) % 8)
        val p0      = if (tileLo & bitSel) 0x01 else 0x00
        val p1      = if (tileHi & bitSel) 0x02 else 0x00
        val pixel   = p1 | p0
        val palette = if (pixel) tileAttr else 0x00
        (pixel, palette)
      } else
        (0x00, 0x00)

    val defaultFg = (0x00, 0x00, SpritePriority.BehindBackground, false)
    val (fgPixel, fgPalette, fgPriority, spriteZeroHit) =
      if (ppu.registers.mask.renderSprites) {
        ppu.spritesState.scanlineOam
          .filter(e => x >= e.sprite.x && x < (e.sprite.x + 8))
          .map { e =>
            val bitSel = 0x80 >> (x - e.sprite.x)
            val p0     = if (e.spriteLo & bitSel) 0x01 else 0x00
            val p1     = if (e.spriteHi & bitSel) 0x02 else 0x00
            val pixel  = p1 | p0
            (pixel, e.sprite.palette, e.sprite.priority, e.isSpriteZero)
          }
          .find { case (pixel, _, _, _) => pixel != 0x00 }
          .getOrElse(defaultFg)
      } else
        defaultFg

    val (pixel, palette) =
      if (fgPixel != 0 && (bgPixel == 0 || fgPriority == SpritePriority.InFrontOfBackground))
        (fgPixel, fgPalette)
      else
        (bgPixel, bgPalette)

    val color = getColor(palette, pixel)(ppu)

    (color, spriteZeroHit)
  }

  def updatePixels(x0: Int, y: Int)(ppu: PpuState): PpuState = {
    if (y >= 0 && y < 240 && x0 >= 0 && x0 < 256) {
      val fineX = ppu.registers.loopy.x
      val bg = ppu.bgRenderingState

      val r1 = fineX until 8
      val r2 = 0 until fineX

      val (pixels1, spriteZeroHit1) =
        r1.foldLeft((ppu.pixels, false)) { case ((pixels, hit), col) =>
          val x = x0 + (col - fineX)
          val (color, possibleSpriteZeroHit) = pixel(x, bg.tileHi, bg.tileLo, bg.tileAttr, ppu)

          val spriteZeroHit = possibleSpriteZeroHit && isRendering(ppu) && y < 258 && x != 255 &&
            ((ppu.registers.mask.renderSpritesLeft || ppu.registers.mask.renderBackgroundLeft) && x > 7)

          (pixels.updated(y * 256 + x, color), hit || spriteZeroHit)
        }

      val (pixels2, spriteZeroHit2) =
        r2.foldLeft((pixels1, spriteZeroHit1)) { case ((pixels, hit), col) =>
          val x = x0 + (8 - fineX) + col
          val (color, possibleSpriteZeroHit) = pixel(x, bg.nextTileHi, bg.nextTileLo, bg.nextTileAttr, ppu)

          val spriteZeroHit = possibleSpriteZeroHit && isRendering(ppu) && y < 258 && x != 255 &&
            ((ppu.registers.mask.renderSpritesLeft || ppu.registers.mask.renderBackgroundLeft) && x > 7)

          (pixels.updated(y * 256 + x, color), hit || spriteZeroHit)
        }

      val update =
        if (spriteZeroHit2)
          PpuState.pixels.set(pixels2) andThen setSpriteZeroHit
        else
          PpuState.pixels.set(pixels2)

      update(ppu)
    } else
      ppu
  }

  def clock(scanline: Int, cycle: Int): Option[State[NesState, NesState]] = {

    def isRenderingPart(scanline: Int): Boolean =
      scanline >= -1 && scanline < 240

    def isFetch(scanline: Int, cycle: Int): Boolean =
      isRenderingPart(scanline) && ((cycle >= 1 && cycle <= 256) || (cycle >= 321 && cycle <= 336))

    def lift(f: PpuState => PpuState): Option[State[NesState, NesState]] =
      Option(
        State { nes =>
          val updated = NesState.ppuState.modify(f)(nes)
          (updated, updated)
        }
      )

    (scanline, cycle) match {
      case (-1, 1) =>
        lift(
          setVerticalBlank(false) _ andThen
          setSpriteOverflow(false) andThen
          clearSpriteZeroHit andThen
          clearScanlineOam
        )

      case (scanline, cycle) if isFetch(scanline, cycle) && (cycle % 8 == 1) =>
        val op = State.get[NesState].flatMap { nes =>
          val ppu = nes.ppuState
          val v = getLoopyV(ppu)
          val x = cycle - 1
          val y = scanline

          val nametableAddress = 0x2000 | (v.asUInt16 & 0x0FFF)
          val nextTileId = readNametables(nametableAddress)(ppu)

          val attrAddress = 0x23C0 | (v.nametables << 10) | ((v.coarseY >> 2) << 3) | (v.coarseX >> 2)
          val shift = (if (v.coarseY & 0x02) 4 else 0) + (if (v.coarseX & 0x02) 2 else 0)
          val nextTileAttr = (readNametables(attrAddress)(ppu) >> shift) & 0x03

          val patternAddress = ppu.registers.ctrl.backgroundTableAddress.address + (nextTileId << 4) + v.fineY
          val nextTile = for {
            nextTileLo <- ppuRead(patternAddress)
            nextTileHi <- ppuRead(patternAddress + 8)
          } yield (nextTileHi, nextTileLo)

          nextTile.transform { case (nes, (nextTileHi, nextTileLo)) =>
            val ppu = nes.ppuState
            val bg = ppu.bgRenderingState
              .loadRegisters
              .setNextTileHi(nextTileHi)
              .setNextTileLo(nextTileLo)
              .setNextTileAttr(nextTileAttr)
            val updated = NesState.ppuState.modify(
              updatePixels(x, y) _ andThen incScrollX andThen setBgRenderingState(bg)
            )(nes)
            (updated, updated)
          }
        }
        Option(op)

      case (scanline, cycle) if isFetch(scanline, cycle) && cycle == 256 =>
        lift(incScrollY)

      case (scanline, cycle) if scanline == -1 && cycle >= 280 && cycle < 305 =>
        lift(transferAddressY)

      case (241, 1) =>
        lift(setVerticalBlank(true))

      case (scanline, 257) if isRenderingPart(scanline) =>
        lift(transferAddressX _ andThen evaluateSprites(scanline))

      case (scanline, 340) if isRenderingPart(scanline) =>
        Option(loadSprites(scanline))

      case _ =>
        None

    }
  }

  def isNmiReady(scanline: Int, cycle: Int, ppu: PpuState): Boolean =
    scanline == 241 && cycle == 2 && ppu.registers.ctrl.nmiMode == NmiMode.On

  def reset: State[NesState, Unit] = State.modify[PpuState](_.reset).toNesState

}
