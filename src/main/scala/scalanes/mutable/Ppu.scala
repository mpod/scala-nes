package scalanes.mutable

import cats.data.Nested
import monocle.Lens
import scalanes.PpuCtrl
import scalanes.mutable.Mirroring.Mirroring
import scalanes.mutable.SpriteOverflow.SpriteOverflow
import scalanes.mutable.SpritePriority.SpritePriority
import scalanes.mutable.VerticalBlank.VerticalBlank

import scala.annotation.tailrec
import scala.language.implicitConversions

class PpuState(
  // Counters
  var cycle: Int,    // 0-340
  var scanline: Int, // 0-261
  var frame: Long,
  // PPU memory
  var nametables: Vector[UInt8],
  var palettes: Vector[UInt8],
  // PPU registers
  var ctrl: UInt8,
  var mask: UInt8,
  var status: UInt8,
  var bufferedData: UInt8,
  // Loopy registers
  var v: UInt15,
  var t: UInt15,
  var x: UInt3,
  var w: UInt1,
  val mirroring: Mirroring,
  // Background
  var shifterPatternLo: UInt16,
  var shifterPatternHi: UInt16,
  var shifterAttrLo: UInt16,
  var shifterAttrHi: UInt16,
  var nextTileId: UInt8,
  var nextTileLo: UInt8,
  var nextTileHi: UInt8,
  var nextTileAttr: UInt8,
  // Sprite
  var spritesState: SpritesState,
  var pixels: Vector[Rgb]
) {
  def reset: PpuState = ???
}

object PpuState {
  val cycle: Lens[PpuState, Int]                 = lens(_.cycle, _.cycle_=)
  val scanline: Lens[PpuState, Int]              = lens(_.scanline, _.scanline_=)
  val frame: Lens[PpuState, Long]                = lens(_.frame, _.frame_=)
  val nametables: Lens[PpuState, Vector[UInt8]]  = lens(_.nametables, _.nametables_=)
  val palettes: Lens[PpuState, Vector[UInt8]]    = lens(_.palettes, _.palettes_=)
  val ctrl: Lens[PpuState, UInt8]                = lens(_.ctrl, _.ctrl_=)
  val mask: Lens[PpuState, UInt8]                = lens(_.mask, _.mask_=)
  val status: Lens[PpuState, UInt8]              = lens(_.status, _.status_=)
  val bufferedData: Lens[PpuState, UInt8]        = lens(_.bufferedData, _.bufferedData_=)
  val v: Lens[PpuState, UInt15]                  = lens(_.v, _.v_=)
  val t: Lens[PpuState, UInt15]                  = lens(_.t, _.t_=)
  val x: Lens[PpuState, UInt3]                   = lens(_.x, _.x_=)
  val w: Lens[PpuState, UInt1]                   = lens(_.w, _.w_=)
  val shifterPatternLo: Lens[PpuState, UInt16]   = lens(_.shifterPatternLo, _.shifterPatternLo_=)
  val shifterPatternHi: Lens[PpuState, UInt16]   = lens(_.shifterPatternHi, _.shifterPatternHi_=)
  val shifterAttrLo: Lens[PpuState, UInt16]      = lens(_.shifterAttrLo, _.shifterAttrLo_=)
  val shifterAttrHi: Lens[PpuState, UInt16]      = lens(_.shifterAttrHi, _.shifterAttrHi_=)
  val nextTileId: Lens[PpuState, UInt8]          = lens(_.nextTileId, _.nextTileId_=)
  val nextTileLo: Lens[PpuState, UInt8]          = lens(_.nextTileLo, _.nextTileLo_=)
  val nextTileHi: Lens[PpuState, UInt8]          = lens(_.nextTileHi, _.nextTileHi_=)
  val nextTileAttr: Lens[PpuState, UInt8]        = lens(_.nextTileAttr, _.nextTileAttr_=)
  val spritesState: Lens[PpuState, SpritesState] = lens(_.spritesState, _.spritesState_=)
  val pixels: Lens[PpuState, Vector[Rgb]]        = lens(_.pixels, _.pixels_=)

  def initial(mirroring: Mirroring): PpuState = ???
}

abstract class RegFlag(mask: Int) extends Enumeration {
  @tailrec
  private def findPos(m: Int, p: Int): Int =
    if ((m & 0x1) == 1) p
    else if (m == 0) p
    else findPos(m >> 1, p + 1)
  val pos: Int                        = findPos(mask, 0)
  def extractFromReg(reg: Int): Value = super.apply((reg & mask) >> pos)
  def modifyReg(v: Value): Int => Int = reg => (reg & ~mask) | ((v.id << pos) & mask)
}

abstract class BooleanRegFlag(mask: Int) extends RegFlag(mask) {
  implicit def valueToBoolean(x: Value): Boolean = x == On
  val On, Off                                    = Value
}

// PPUCTRL flags
object NametableAddress extends RegFlag(0x03) {
  type NametableAddress = Val
  protected case class Val(address: UInt16) extends super.Val
  implicit def valueToVal(x: Value): Val = x.asInstanceOf[Val]
  val First: NametableAddress            = Val(0x2000)
  val Second: NametableAddress           = Val(0x2400)
  val Third: NametableAddress            = Val(0x2800)
  val Fourth: NametableAddress           = Val(0x2c00)
}

object AddressIncrementMode extends RegFlag(0x1 << 2) {
  type AddressIncrementMode = Val
  protected case class Val(delta: Int) extends super.Val
  implicit def valueToVal(x: Value): Val = x.asInstanceOf[Val]
  val Add1: AddressIncrementMode         = Val(1)
  val Add32: AddressIncrementMode        = Val(32)
}

object SpriteTableAddress extends RegFlag(0x1 << 3) {
  type SpriteTableAddress = Val
  protected case class Val(address: UInt16) extends super.Val
  implicit def valueToVal(x: Value): Val = x.asInstanceOf[Val]
  val First: SpriteTableAddress          = Val(0x0000)
  val Second: SpriteTableAddress         = Val(0x1000)
}

object BackgroundTableAddress extends RegFlag(0x1 << 4) {
  type BackgroundTableAddress = Val
  protected case class Val(address: UInt16) extends super.Val
  implicit def valueToVal(x: Value): Val = x.asInstanceOf[Val]
  val First: BackgroundTableAddress      = Val(0x0000)
  val Second: BackgroundTableAddress     = Val(0x1000)
}

object SpriteSize extends RegFlag(0x1 << 5) {
  type SpriteSize = Val
  protected case class Val(height: Int) extends super.Val
  implicit def valueToVal(x: Value): Val = x.asInstanceOf[Val]
  val Small: SpriteSize                  = Val(8)
  val Large: SpriteSize                  = Val(16)
}

object MasterSlaveMode extends RegFlag(0x1 << 6) {
  type MasterSlaveMode = Value
  val ReadBackdropFromExtPins, OutputColorOnExtPins = Value
}

object NmiMode extends BooleanRegFlag(0x1 << 7) {
  type NmiMode = Value
}

// PPUMASK flags
object Greyscale extends BooleanRegFlag(0x1) {
  type Greyscale = Value
}

object RenderBackgroundLeft extends BooleanRegFlag(0x1 << 1) {
  type RenderBackgroundLeft = Value
}

object RenderSpritesLeft extends BooleanRegFlag(0x1 << 2) {
  type RenderSpritesLeft = Value
}

object RenderBackground extends BooleanRegFlag(0x1 << 3) {
  type RenderBackground = Value
}

object RenderSprites extends BooleanRegFlag(0x1 << 4) {
  type RenderSprites = Value
}

object EnhanceRed extends BooleanRegFlag(0x1 << 5) {
  type EnhanceRed = Value
}

object EnhanceGreen extends BooleanRegFlag(0x1 << 6) {
  type EnhanceGreen = Value
}

object EnhanceBlue extends BooleanRegFlag(0x1 << 7) {
  type EnhanceBlue = Value
}

// PPUSTATUS flags
object SpriteOverflow extends BooleanRegFlag(0x1 << 5) {
  type SpriteOverflow = Value
}

object SpriteZeroHit extends BooleanRegFlag(0x1 << 6) {
  type SpriteZeroHit = Value
}

object VerticalBlank extends BooleanRegFlag(0x1 << 7) {
  type VerticalBlank = Value
}

object Loopy {
  private val posCoarseX     = 0
  private val maskCoarseX    = 0x1f << posCoarseX
  private val posCoarseY     = 5
  private val maskCoarseY    = 0x1f << posCoarseY
  private val posNametableX  = 10
  private val maskNametableX = 0x1 << posNametableX
  private val posNametableY  = 11
  private val maskNametableY = 0x1 << posNametableY
  private val posFineY       = 12
  private val maskFineY      = 0x7 << posFineY

  private def setBits(d: Int, n: Int, mask: Int, pos: Int): Int => Int =
    reg => {
      val typeMask = (0 until n).map(1 << _).reduce(_ | _)
      require((d & typeMask) == d)
      (reg & ~mask) | (d << pos)
    }

  def setCoarseX(d: UInt5): UInt15 => UInt15    = setBits(d, 5, maskCoarseX, posCoarseX)
  def setCoarseY(d: UInt5): UInt15 => UInt15    = setBits(d, 5, maskCoarseY, posCoarseY)
  def setNametableX(d: UInt1): UInt15 => UInt15 = setBits(d, 1, maskNametableX, posNametableX)
  def setNametableY(d: UInt1): UInt15 => UInt15 = setBits(d, 1, maskNametableY, posNametableY)
  def setNametables(d: UInt2): UInt15 => UInt15 = setBits(d, 2, maskNametableX | maskNametableY, posNametableX)
  def setFineY(d: UInt3): UInt15 => UInt15      = setBits(d, 1, maskFineY, posFineY)
  val flipNametableX: UInt15 => UInt15          = loopy => loopy ^ (0x1 << posNametableX)
  val flipNametableY: UInt15 => UInt15          = loopy => loopy ^ (0x1 << posNametableY)
  val coarseX: UInt15 => UInt5                  = loopy => (loopy & maskCoarseX) >> posCoarseX
  val coarseY: UInt15 => UInt5                  = loopy => (loopy & maskCoarseY) >> posCoarseY
  val nametableX: UInt15 => UInt1               = loopy => (loopy & maskNametableX) >> posNametableX
  val nametableY: UInt15 => UInt1               = loopy => (loopy & maskNametableY) >> posNametableY
  val fineY: UInt15 => UInt3                    = loopy => (loopy & maskFineY) >> posFineY
}

class BgRenderingState(
  var tileLo: UInt8,
  var tileHi: UInt8,
  var tileAttr: UInt8,
  var nextTileLo: UInt8,
  var nextTileHi: UInt8,
  var nextTileAttr: UInt8
) {
  def loadRegisters: BgRenderingState = {
    tileLo = nextTileLo
    tileHi = nextTileHi
    tileAttr = nextTileAttr
    this
  }

  def setNextTileAttr(d: UInt8): BgRenderingState = {
    nextTileAttr = d
    this
  }

  def setNextTileLo(d: UInt8): BgRenderingState = {
    nextTileLo = d
    this
  }

  def setNextTileHi(d: UInt8): BgRenderingState = {
    nextTileHi = d
    this
  }

  def reset: BgRenderingState = BgRenderingState.initial
}

object BgRenderingState {
  def initial: BgRenderingState =
    new BgRenderingState(0, 0, 0, 0, 0, 0)
}

object Mirroring extends Enumeration {
  type Mirroring = Value
  val Vertical, Horizontal, OneScreenLowerBank, OneScreenUpperBank = Value
}

case class Rgb(r: Int, g: Int, b: Int)

object Rgb {
  // format: off
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
  // format: on
  val initial: Rgb = Rgb(0, 0, 0)
}

class OamEntry(
  var y: UInt8,
  var id: UInt8,
  var attribute: UInt8,
  var x: UInt8
) {
  def readField(i: Int): UInt8 = i match {
    case 0 => y
    case 1 => id
    case 2 => attribute
    case 3 => x
    case _ => throw new IllegalArgumentException("Index should be in range 0..3")
  }

  def writeField(i: Int, d: UInt8): OamEntry =
    (
      i match {
        case 0 => OamEntry.y.set(d)
        case 1 => OamEntry.id.set(d)
        case 2 => OamEntry.attribute.set(d)
        case 3 => OamEntry.x.set(d)
        case _ =>
          throw new IllegalArgumentException("Index should be in range 0..3")
      }
    )(this)

  def palette: UInt3            = (attribute & 0x03) + 4
  def priority: SpritePriority  = SpritePriority(attribute & 0x10)
  def flipHorizontally: Boolean = attribute & 0x40
  def flipVertically: Boolean   = attribute & 0x80
}

object OamEntry {
  val y: Lens[OamEntry, UInt8]         = lens(_.y, _.y_=)
  val x: Lens[OamEntry, UInt8]         = lens(_.x, _.x_=)
  val id: Lens[OamEntry, UInt8]        = lens(_.id, _.id_=)
  val attribute: Lens[OamEntry, UInt8] = lens(_.attribute, _.attribute_=)
  val initial: OamEntry                = new OamEntry(0x00, 0x00, 0x00, 0x00)
}

class ScanlineOamEntry(
  var sprite: OamEntry,
  var spriteLo: UInt8,
  var spriteHi: UInt8,
  var isSpriteZero: Boolean
) {

  def shiftRegisters: ScanlineOamEntry =
    (
      ScanlineOamEntry.spriteLo.modify(spriteLo => (spriteLo << 1) & 0xff) andThen
        ScanlineOamEntry.spriteHi.modify(spriteHi => (spriteHi << 1) & 0xff)
    )(this)
}

object ScanlineOamEntry {
  val sprite: Lens[ScanlineOamEntry, OamEntry] = lens(_.sprite, _.sprite_=)
  val spriteLo: Lens[ScanlineOamEntry, UInt8]  = lens(_.spriteLo, _.spriteLo_=)
  val spriteHi: Lens[ScanlineOamEntry, UInt8]  = lens(_.spriteHi, _.spriteHi_=)

  def apply(sprite: OamEntry, isZeroSprite: Boolean): ScanlineOamEntry =
    new ScanlineOamEntry(sprite, 0x00, 0x00, isZeroSprite)
}

object SpritePriority extends Enumeration {
  type SpritePriority = Value
  val InFrontOfBackground, BehindBackground = Value
}

class SpritesState(
  var oam: Vector[OamEntry],
  var oamAddress: UInt8,
  var scanlineOam: Vector[ScanlineOamEntry]
)

object SpritesState {
  val oam: Lens[SpritesState, Vector[OamEntry]]                 = lens(_.oam, _.oam_=)
  val oamAddress: Lens[SpritesState, UInt8]                     = lens(_.oamAddress, _.oamAddress_=)
  val scanlineOam: Lens[SpritesState, Vector[ScanlineOamEntry]] = lens(_.scanlineOam, _.scanlineOam_=)

  val initial: SpritesState = new SpritesState(Vector.fill(64)(OamEntry.initial), 0x00, Vector.empty)
}

object Ppu {

  val noOp: NesState => NesState = nes => nes

  private def lift(f: PpuState => PpuState): NesState => NesState =
    NesState.ppuState.modify(f)

  def liftS(f: PpuState => PpuState): State[NesState, Unit] =
    State.modify(NesState.ppuState.modify(f))

  def liftS2(f: PpuState => PpuState): State[NesState, NesState] =
    State.get[NesState].map(NesState.ppuState.modify(f))

  def setVerticalBlank(d: VerticalBlank): PpuState => PpuState =
    PpuState.status.modify(VerticalBlank.modifyReg(d))

  val setSpriteZeroHit: PpuState => PpuState =
    PpuState.status.modify(SpriteZeroHit.modifyReg(SpriteZeroHit.On))

  val clearSpriteZeroHit: PpuState => PpuState =
    PpuState.status.modify(SpriteZeroHit.modifyReg(SpriteZeroHit.Off))

  def setSpriteOverflow(d: SpriteOverflow): PpuState => PpuState =
    PpuState.status.modify(SpriteOverflow.modifyReg(d))

//  def clearScanlineOam(ppu: PpuState): PpuState =
//    (PpuState.spritesState composeLens SpritesState.scanlineOam).set(Vector.empty)(ppu)

  val clearLoopyW: PpuState => PpuState =
    PpuState.w.set(0x0)

  val incScrollX: PpuState => PpuState =
    ppu => {
      val coarseX = Loopy.coarseX(ppu.v)
      PpuState.v.modify {
        if (coarseX == 31)
          Loopy.setCoarseX(0) andThen Loopy.flipNametableX
        else
          Loopy.setCoarseX(coarseX + 1)
      }(ppu)
    }

  val incScrollY: PpuState => PpuState =
    ppu => {
      val coarseY = Loopy.coarseY(ppu.v)
      val fineY   = Loopy.fineY(ppu.v)
      PpuState.v.modify {
        if (fineY < 7)
          Loopy.setFineY(fineY + 1)
        else if (coarseY == 29)
          Loopy.setFineY(0) andThen Loopy.setCoarseY(0) andThen Loopy.flipNametableY
        else if (coarseY == 31)
          Loopy.setFineY(0) andThen Loopy.setCoarseY(0)
        else
          Loopy.setFineY(0) andThen Loopy.setCoarseY(coarseY + 1)
      }(ppu)
    }

  val transferAddressX: PpuState => PpuState =
    ppu => {
      val tCoarseX    = Loopy.coarseX(ppu.t)
      val tNametableX = Loopy.nametableX(ppu.t)
      PpuState.v.modify {
        Loopy.setCoarseX(tCoarseX) andThen Loopy.setNametableX(tNametableX)
      }(ppu)
    }

  val transferAddressY: PpuState => PpuState =
    ppu => {
      val tFineY      = Loopy.fineY(ppu.t)
      val tNametableY = Loopy.nametableY(ppu.t)
      val tCoarseY    = Loopy.coarseY(ppu.t)
      PpuState.v.modify {
        Loopy.setFineY(tFineY) andThen Loopy.setNametableY(tNametableY) andThen Loopy.setCoarseY(tCoarseY)
      }(ppu)
    }

  val loadShifters: PpuState => PpuState =
    ppu => {
      val shifterPatternLo = (ppu.shifterPatternLo & 0xff00) | ppu.nextTileLo
      val shifterPatternHi = (ppu.shifterPatternHi & 0xff00) | ppu.nextTileHi
      val shifterAttrLo    = (ppu.shifterPatternLo & 0xff00) | (if (ppu.nextTileAttr & 0x01) 0xff else 0x00)
      val shifterAttrHi    = (ppu.shifterPatternHi & 0xff00) | (if (ppu.nextTileAttr & 0x02) 0xff else 0x00)
      (PpuState.shifterPatternLo.set(shifterPatternLo) andThen
        PpuState.shifterPatternHi.set(shifterPatternHi) andThen
        PpuState.shifterAttrLo.set(shifterAttrLo) andThen
        PpuState.shifterAttrHi.set(shifterAttrHi))(ppu)
    }

  val updateShifters: PpuState => PpuState =
    PpuState.shifterPatternLo.modify(_ << 1) andThen
      PpuState.shifterPatternHi.modify(_ << 1) andThen
      PpuState.shifterAttrLo.modify(_ << 1) andThen
      PpuState.shifterAttrHi.modify(_ << 1)

  // TODO: docs
  val fetchNametableByte: NesState => NesState =
    nes => {
      val address   = 0x2000 | (nes.ppuState.v & 0x0fff)
      val (nes1, d) = ppuRead(address)(nes)
      lift(PpuState.nextTileId.set(d))(nes1)
    }

  // TODO: docs
  val fetchAttributeTableByte: NesState => NesState =
    nes => {
      val v         = nes.ppuState.v
      val address   = 0x23c0 | (v & 0x0c00) | ((v >> 4) & 0x38) | ((v >> 2) & 0x07)
      val shift     = ((v >> 4) & 4) | (v & 2)
      val (nes1, d) = ppuRead(address)(nes)
      val attr      = (d >> shift) & 0x3
      lift(PpuState.nextTileAttr.set(attr))(nes1)
    }

  // TODO: docs
  val fetchLowTileByte: NesState => NesState =
    nes => {
      val ppu                    = nes.ppuState
      val backgroundTableAddress = BackgroundTableAddress.extractFromReg(ppu.ctrl).address
      val address                = (backgroundTableAddress << 12) + (ppu.nextTileId << 4) + Loopy.fineY(ppu.v)
      ppuRead(address).runS(nes)
    }

  // TODO: docs
  val fetchHighTileByte: NesState => NesState =
    nes => {
      val ppu                    = nes.ppuState
      val backgroundTableAddress = BackgroundTableAddress.extractFromReg(ppu.ctrl).address
      val address                = (backgroundTableAddress << 12) + (ppu.nextTileId << 4) + Loopy.fineY(ppu.v) + 8
      ppuRead(address).runS(nes)
    }

  // TODO: documentation
  private def mapToNametableIndex(address: UInt16, mirroring: Mirroring): UInt16 = {
    val addr = address & 0x0fff
    mirroring match {
      case Mirroring.Vertical   => addr & 0x07ff
      case Mirroring.Horizontal => ((addr & 0x800) >> 1) | (addr & 0x3ff)
    }
  }

  def readNametables(address: UInt16)(ppu: PpuState): UInt8 = {
    require((address & 0xffff) == address)
    require(address >= 0x2000 && address < 0x3f00)

    val i = mapToNametableIndex(address, ppu.mirroring)
    ppu.nametables(i)
  }

  def writeNametables(address: UInt16, d: UInt8)(ppu: PpuState): PpuState = {
    require((address & 0xffff) == address)
    require(address >= 0x2000 && address < 0x3f00)

    val i = mapToNametableIndex(address, ppu.mirroring)
    PpuState.nametables.modify(_.updated(i, d))(ppu)
  }

  // TODO: documentation
  private def mapToPalettesIndex(address: UInt16): UInt16 = {
    val addr = address & 0x1f
    // Mirror $3F10, $3F14, $3F18, $3F1C to $3F00, $3F04, $3F08, $3F0C
    if ((addr & 0x13) == 0x10) addr & ~0x10 else addr
  }

  def getColor(palette: Int, pixel: Int)(ppu: PpuState): Rgb = {
    require(palette >= 0 && palette < 8)
    require(pixel >= 0 && pixel < 4)

    val colorAddress = 0x3f00 + (palette << 2) + pixel
    val colorValue =
      if (RenderBackground.extractFromReg(ppu.mask))
        ppu.palettes(mapToPalettesIndex(colorAddress))
      else
        ppu.palettes(mapToPalettesIndex(0x0000))
    Rgb.palette(colorValue)
  }

  def readPalettes(address: UInt16)(ppu: PpuState): UInt8 = {
    require((address & 0xffff) == address)
    require(address >= 0x3f00 && address < 0x4000, s"failed: $address")

    val addr  = mapToPalettesIndex(address)
    val color = ppu.palettes(addr)
    if (Greyscale.extractFromReg(ppu.mask)) color & 0x30
    else color
  }

  def writePalettes(address: UInt16, d: UInt8)(ppu: PpuState): PpuState = {
    require((address & 0xffff) == address)
    require(address >= 0x3f00 && address < 0x3fff)

    val addr = mapToPalettesIndex(address)
    PpuState.palettes.modify(_.updated(addr, d))(ppu)
  }

  /*
  def readOam: State[NesState, UInt8] = State.inspect { nes =>
    val oamAddress = nes.ppuState.spritesState.oamAddress
    val entryIndex = oamAddress / 4
    val field      = oamAddress % 4
    nes.ppuState.spritesState.oam(entryIndex).readField(field)
  }

  def writeOam(oamAddress: UInt8, d: UInt8): State[NesState, Unit] = {
    val entryIndex = oamAddress / 4
    val field      = oamAddress % 4
    val update = (PpuState.spritesState composeLens SpritesState.oam composeOptional index(entryIndex)).modify(
      _.writeField(field, d)
    )
    modifyNesState(update)
  }

  def evaluateSprites(scanline: Int)(ppu: PpuState): PpuState = {
    val spriteSize = ppu.registers.ctrl.spriteSize
    val affectedSprites = ppu.spritesState.oam.zipWithIndex
      .filter { case (e, _) =>
        val diff = scanline - e.y
        diff >= 0 && diff < spriteSize.height
      }
      .map { case (e, i) =>
        ScanlineOamEntry(e, i == 0)
      }
    val spriteOverflow  = affectedSprites.size > 8
    val scanlineSprites = affectedSprites.take(8)

    val update =
      (PpuState.spritesState composeLens SpritesState.scanlineOam).set(scanlineSprites) andThen
        (statusRegister composeLens PpuStatus.spriteOverflow).set(spriteOverflow)

    update(ppu)
  }

  private def flipByte(d: UInt8): UInt8 = {
    val d1 = (d & 0xf0) >> 4 | (d & 0x0f) << 4
    val d2 = (d1 & 0xcc) >> 2 | (d1 & 0x33) << 2
    val d3 = (d2 & 0xaa) >> 1 | (d2 & 0x55) << 1
    d3
  }

  def loadSprites(scanline: Int): State[NesState, NesState] = State.get[NesState].flatMap { nes =>
    val ppu = nes.ppuState
    ppu.spritesState.scanlineOam.zipWithIndex
      .map { case (e, i) =>
        val spriteSize = ppu.registers.ctrl.spriteSize
        val addrBase =
          if (spriteSize == SpriteSize.Small)
            ppu.registers.ctrl.spriteTableAddress.address | (e.sprite.id << 4)
          else
            ((e.sprite.id & 0x01) << 12) | ((e.sprite.id & 0xfe) << 4)
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
   */

  def cpuRead(address: UInt16): State[NesState, UInt8] =
    nes => {
      // The PPU exposes eight memory-mapped registers to the CPU, which are mapped to the
      // address range $2000-$2007. Addresses between $2008 and $3FFF are mirrored.
      require(address >= 0x2000 && address <= 0x3fff)
      address & 0x0007 match {
        case 0x0002 => // PPUSTATUS
          readStatus.run(nes)
        case 0x0004 => // OAMDATA
          ??? // readOam
        case 0x0007 => // PPUDATA
          readData.run(nes)
        case _ =>
          (nes, 0x00)
      }
    }

  def cpuWrite(address: UInt16, d: UInt8): NesState => NesState = {
    require(address >= 0x2000 && address <= 0x3fff)
    require((d & 0xff) == d)
    address & 0x0007 match {
      case 0x0000 => writeCtrl(d)
      case 0x0001 => writeMask(d)
      case 0x0002 => identity
      case 0x0003 => ???
      case 0x0004 => ??? // writeOam(ppu.spritesState.oamAddress, d)
      case 0x0005 => writeScroll(d)
      case 0x0006 => writeAddr(d)
      case 0x0007 => writeData(d)
    }
  }

  def writeCtrl(d: UInt8): NesState => NesState =
    nes => {
      val nametables = NametableAddress.extractFromReg(d).id
      val t          = Loopy.setNametables(nametables)(nes.ppuState.t)
      lift(PpuState.ctrl.set(d) andThen PpuState.t.set(t))(nes)
    }

  def writeMask(d: UInt8): NesState => NesState =
    lift(PpuState.mask.set(d))

  def writeScroll(d: UInt8): NesState => NesState =
    nes =>
      if (nes.ppuState.w) {
        // Set coarseY and fineY
        val t = (nes.ppuState.t & 0x8c1f) | ((d & 0x07) << 12) | ((d & 0xf8) << 2)
        lift(PpuState.t.set(t) andThen PpuState.w.set(0))(nes)
      } else {
        // Set coarseX and x
        val t = (nes.ppuState.t & 0xffe0) | (d >> 3)
        val x = d & 0x7
        lift(PpuState.t.set(t) andThen PpuState.x.set(x) andThen PpuState.w.set(1))(nes)
      }

  def writeAddr(d: UInt8): NesState => NesState =
    nes => {
      if (nes.ppuState.w) {
        // Upper address byte
        val t = (nes.ppuState.t & 0xff00) | d
        lift(PpuState.t.set(t) andThen PpuState.v.set(t) andThen PpuState.w.set(0))(nes)
      } else {
        // Lower address byte
        val t = (nes.ppuState.t & 0x80ff) | (d & 0x3f)
        lift(PpuState.t.set(t) andThen PpuState.w.set(1))(nes)
      }
    }

  def writeData(d: UInt8): NesState => NesState =
    nes => {
      val delta = AddressIncrementMode.extractFromReg(nes.ppuState.ctrl).delta
      (ppuWrite(nes.ppuState.v, d) andThen lift(PpuState.v.modify(_ + delta)))(nes)
    }

  val readStatus: State[NesState, UInt8] =
    nes => {
      val d    = (nes.ppuState.status & 0xe0) | (nes.ppuState.bufferedData & 0x1f)
      val nes1 = lift(clearLoopyW andThen setVerticalBlank(VerticalBlank.Off))(nes)
      (nes1, d)
    }

  val readData: State[NesState, UInt8] =
    State.get[NesState].flatMap { nes =>
      val fetch =
        // Address is below palettes
        if ((nes.ppuState.v & 0x3fff) < 0x3f00) {
          // Take data from the buffer
          ppuRead(nes.ppuState.v).map(buffer => (buffer, nes.ppuState.bufferedData))
        } else {
          // Take data from VRAM
          for {
            d      <- ppuRead(nes.ppuState.v)
            buffer <- ppuRead(nes.ppuState.v - 0x1000)
          } yield (buffer, d)
        }
      for {
        result <- fetch
        (buffer, d) = result
        nes <- liftS2(PpuState.bufferedData.set(buffer))
        delta = AddressIncrementMode.extractFromReg(nes.ppuState.ctrl).delta
        _ <- liftS(PpuState.v.modify(_ + delta))
      } yield d
    }

  def ppuRead(address: UInt16): State[NesState, UInt8] = {
    require((address & 0x3fff) == address)

    if (address >= 0x0000 && address <= 0x1fff)
      Cartridge.ppuRead(address) // Patterns
    else if (address >= 0x2000 && address <= 0x3eff)
      nes => (nes, readNametables(address)(nes.ppuState))
    else
      nes => (nes, readPalettes(address)(nes.ppuState))
  }

  def ppuWrite(address: UInt16, d: UInt8): NesState => NesState = {
    require((address & 0x3fff) == address)

    if (address >= 0x0000 && address <= 0x1fff)
      Cartridge.ppuWrite(address, d).runS(_) // Patterns
    else if (address >= 0x2000 && address <= 0x3eff)
      lift(writeNametables(address, d))
    else
      lift(writePalettes(address, d))
  }

  /*
  private def pixel(x: Int, tileHi: UInt8, tileLo: UInt8, tileAttr: UInt2, ppu: PpuState): (Rgb, Boolean) = {

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

  def updatePixels(x0: Int, y: Int)(ppu: PpuState): PpuState =
    if (y >= 0 && y < 240 && x0 >= 0 && x0 < 256) {
      val fineX = ppu.registers.loopy.x
      val bg    = ppu.bgRenderingState

      val r1 = fineX until 8
      val r2 = 0 until fineX

      val (pixels1, spriteZeroHit1) =
        r1.foldLeft((ppu.pixels, false)) { case ((pixels, hit), col) =>
          val x                              = x0 + (col - fineX)
          val (color, possibleSpriteZeroHit) = pixel(x, bg.tileHi, bg.tileLo, bg.tileAttr, ppu)

          val spriteZeroHit = possibleSpriteZeroHit && isRendering(ppu) && y < 258 && x != 255 &&
            ((ppu.registers.mask.renderSpritesLeft || ppu.registers.mask.renderBackgroundLeft) && x > 7)

          (pixels.updated(y * 256 + x, color), hit || spriteZeroHit)
        }

      val (pixels2, spriteZeroHit2) =
        r2.foldLeft((pixels1, spriteZeroHit1)) { case ((pixels, hit), col) =>
          val x                              = x0 + (8 - fineX) + col
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
   */

  def incCounters: PpuState => PpuState =
    ppu => {
      val n = ppu.scanline * 341 + ppu.cycle + 1
      val (cycle, scanline, frame) =
        if (n >= 341 * 262) (0, 0, ppu.frame + 1)
        else if (n % 341 == 0) (0, ppu.scanline + 1, ppu.frame)
        else (ppu.cycle + 1, ppu.scanline, ppu.frame)
      (PpuState.cycle.set(cycle) andThen PpuState.scanline.set(scanline) andThen PpuState.frame.set(frame))(ppu)
    }

  def clock: NesState => NesState =
    nes => {
      val ppu             = nes.ppuState
      val scanline        = ppu.scanline
      val cycle           = ppu.cycle
      val isVisibleLine   = scanline < 240
      val isPreRenderLine = scanline == 261
      val isRenderLine    = isVisibleLine || isPreRenderLine
      val isRendering     = RenderBackground.extractFromReg(ppu.mask) || RenderSprites.extractFromReg(ppu.mask)
      val isPreFetchCycle = cycle >= 321 && cycle <= 336
      val isVisibleCycle  = cycle >= 1 && cycle <= 256
      val isFetchCycle    = isPreFetchCycle || isVisibleCycle

      // Background rendering
      val backgroundRendering =
        if (isRendering && isRenderLine) {
          val f1 =
            if (isVisibleLine && isVisibleCycle) noOp // render a pixel
            else noOp
          val f2 =
            if (isFetchCycle) {
              cycle % 8 match {
                case 1                 => lift(updateShifters) andThen fetchNametableByte
                case 3                 => lift(updateShifters) andThen fetchAttributeTableByte
                case 5                 => lift(updateShifters) andThen fetchLowTileByte
                case 7                 => lift(updateShifters) andThen fetchHighTileByte
                case 0 if cycle == 256 => lift(updateShifters andThen loadShifters andThen incScrollY)
                case 0                 => lift(updateShifters andThen loadShifters andThen incScrollX)
                case _                 => lift(updateShifters)
              }
            } else if (isPreRenderLine && cycle >= 280 && cycle <= 304) lift(transferAddressY)
            else if (cycle == 257) lift(transferAddressX)
            else noOp
          f1 andThen f2
        } else noOp

      // Sprite rendering
      ???

      backgroundRendering(nes)
    }

  def isNmiReady(scanline: Int, cycle: Int, ppu: PpuState): Boolean = ???

  def reset: State[NesState, Unit] = ???
}
