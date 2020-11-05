package scalanes.mutable

import monocle.Lens
import scalanes.mutable.Mirroring.Mirroring
import scalanes.mutable.SpritePriority.SpritePriority

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
  var oamData: Vector[UInt8],
  var oamAddress: UInt16,
  var spritesInfo: Seq[SpriteInfo],
  // Other
  val canvas: Array[Int],
  val mirroring: Mirroring
)

object PpuState {
  val nametables: Lens[PpuState, Vector[UInt8]]    = lens(_.nametables, _.nametables_=)
  val palettes: Lens[PpuState, Vector[UInt8]]      = lens(_.palettes, _.palettes_=)
  val ctrl: Lens[PpuState, UInt8]                  = lens(_.ctrl, _.ctrl_=)
  val mask: Lens[PpuState, UInt8]                  = lens(_.mask, _.mask_=)
  val status: Lens[PpuState, UInt8]                = lens(_.status, _.status_=)
  val bufferedData: Lens[PpuState, UInt8]          = lens(_.bufferedData, _.bufferedData_=)
  val v: Lens[PpuState, UInt15]                    = lens(_.v, _.v_=)
  val t: Lens[PpuState, UInt15]                    = lens(_.t, _.t_=)
  val x: Lens[PpuState, UInt3]                     = lens(_.x, _.x_=)
  val w: Lens[PpuState, UInt1]                     = lens(_.w, _.w_=)
  val shifterPatternLo: Lens[PpuState, UInt16]     = lens(_.shifterPatternLo, _.shifterPatternLo_=)
  val shifterPatternHi: Lens[PpuState, UInt16]     = lens(_.shifterPatternHi, _.shifterPatternHi_=)
  val shifterAttrLo: Lens[PpuState, UInt16]        = lens(_.shifterAttrLo, _.shifterAttrLo_=)
  val shifterAttrHi: Lens[PpuState, UInt16]        = lens(_.shifterAttrHi, _.shifterAttrHi_=)
  val nextTileId: Lens[PpuState, UInt8]            = lens(_.nextTileId, _.nextTileId_=)
  val nextTileLo: Lens[PpuState, UInt8]            = lens(_.nextTileLo, _.nextTileLo_=)
  val nextTileHi: Lens[PpuState, UInt8]            = lens(_.nextTileHi, _.nextTileHi_=)
  val nextTileAttr: Lens[PpuState, UInt8]          = lens(_.nextTileAttr, _.nextTileAttr_=)
  val oamData: Lens[PpuState, Vector[UInt8]]       = lens(_.oamData, _.oamData_=)
  val oamAddress: Lens[PpuState, UInt16]           = lens(_.oamAddress, _.oamAddress_=)
  val spritesInfo: Lens[PpuState, Seq[SpriteInfo]] = lens(_.spritesInfo, _.spritesInfo_=)

  def apply(mirroring: Mirroring): PpuState = new PpuState(
    cycle = 0,
    scanline = 0,
    frame = 0,
    nametables = Vector.fill(2 * 1024)(0x00),
    palettes = Vector.fill(32)(0x00),
    ctrl = 0x00,
    mask = 0x00,
    status = 0x00,
    bufferedData = 0,
    v = 0x0000,
    t = 0x0000,
    x = 0x0,
    w = 0x0,
    shifterPatternLo = 0x0000,
    shifterPatternHi = 0x0000,
    shifterAttrLo = 0x0000,
    shifterAttrHi = 0x0000,
    nextTileId = 0x00,
    nextTileLo = 0x00,
    nextTileHi = 0x00,
    nextTileAttr = 0x00,
    oamData = Vector.fill(256)(0x00),
    oamAddress = 0x0000,
    spritesInfo = List.empty,
    canvas = Array.fill(2 * 256 * 2 * 240)(0),
    mirroring = mirroring
  )
}

abstract class RegFlag(mask: Int, lens: Lens[PpuState, UInt8]) extends Enumeration {
  @tailrec
  private def findPos(m: Int, p: Int): Int =
    if ((m & 0x1) == 1) p
    else if (m == 0) p
    else findPos(m >> 1, p + 1)
  val pos: Int                               = findPos(mask, 0)
  def get(v: UInt8): Value                   = super.apply((v & mask) >> pos)
  def get: PpuState => Value                 = ppu => get(lens.get(ppu))
  def modify(v: Value): PpuState => PpuState = lens.modify(reg => (reg & ~mask) | ((v.id << pos) & mask))
}

abstract class BooleanRegFlag(mask: Int, lens: Lens[PpuState, UInt8]) extends RegFlag(mask, lens) {
  implicit def valueToBoolean(x: Value): Boolean = x == On
  val Off, On                                    = Value
}

// PPUCTRL flags
object NametableAddress extends RegFlag(0x03, PpuState.ctrl) {
  type NametableAddress = Val
  protected case class Val(address: UInt16) extends super.Val
  implicit def valueToVal(x: Value): Val = x.asInstanceOf[Val]
  val First: NametableAddress            = Val(0x2000)
  val Second: NametableAddress           = Val(0x2400)
  val Third: NametableAddress            = Val(0x2800)
  val Fourth: NametableAddress           = Val(0x2c00)
}

object AddressIncrementMode extends RegFlag(0x1 << 2, PpuState.ctrl) {
  type AddressIncrementMode = Val
  protected case class Val(delta: Int) extends super.Val
  implicit def valueToVal(x: Value): Val = x.asInstanceOf[Val]
  val Add1: AddressIncrementMode         = Val(1)
  val Add32: AddressIncrementMode        = Val(32)
}

object SpriteTableAddress extends RegFlag(0x1 << 3, PpuState.ctrl) {
  type SpriteTableAddress = Val
  protected case class Val(address: UInt16) extends super.Val
  implicit def valueToVal(x: Value): Val = x.asInstanceOf[Val]
  val First: SpriteTableAddress          = Val(0x0000)
  val Second: SpriteTableAddress         = Val(0x1000)
}

object BackgroundTableAddress extends RegFlag(0x1 << 4, PpuState.ctrl) {
  type BackgroundTableAddress = Val
  protected case class Val(address: UInt16) extends super.Val
  implicit def valueToVal(x: Value): Val = x.asInstanceOf[Val]
  val First: BackgroundTableAddress      = Val(0x0000)
  val Second: BackgroundTableAddress     = Val(0x1000)
}

object SpriteSize extends RegFlag(0x1 << 5, PpuState.ctrl) {
  type SpriteSize = Val
  protected case class Val(height: Int) extends super.Val
  implicit def valueToVal(x: Value): Val = x.asInstanceOf[Val]
  val Small: SpriteSize                  = Val(8)
  val Large: SpriteSize                  = Val(16)
}

object MasterSlaveMode extends RegFlag(0x1 << 6, PpuState.ctrl) {
  type MasterSlaveMode = Value
  val ReadBackdropFromExtPins, OutputColorOnExtPins = Value
}

object NmiMode extends BooleanRegFlag(0x1 << 7, PpuState.ctrl) {
  type NmiMode = Value
}

// PPUMASK flags
object Greyscale extends BooleanRegFlag(0x1, PpuState.mask) {
  type Greyscale = Value
}

object RenderBackgroundLeft extends BooleanRegFlag(0x1 << 1, PpuState.mask) {
  type RenderBackgroundLeft = Value
}

object RenderSpritesLeft extends BooleanRegFlag(0x1 << 2, PpuState.mask) {
  type RenderSpritesLeft = Value
}

object RenderBackground extends BooleanRegFlag(0x1 << 3, PpuState.mask) {
  type RenderBackground = Value
}

object RenderSprites extends BooleanRegFlag(0x1 << 4, PpuState.mask) {
  type RenderSprites = Value
}

object EnhanceRed extends BooleanRegFlag(0x1 << 5, PpuState.mask) {
  type EnhanceRed = Value
}

object EnhanceGreen extends BooleanRegFlag(0x1 << 6, PpuState.mask) {
  type EnhanceGreen = Value
}

object EnhanceBlue extends BooleanRegFlag(0x1 << 7, PpuState.mask) {
  type EnhanceBlue = Value
}

// PPUSTATUS flags
object SpriteOverflow extends BooleanRegFlag(0x1 << 5, PpuState.status) {
  type SpriteOverflow = Value
}

object SpriteZeroHit extends BooleanRegFlag(0x1 << 6, PpuState.status) {
  type SpriteZeroHit = Value
}

object VerticalBlank extends BooleanRegFlag(0x1 << 7, PpuState.status) {
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
  def setFineY(d: UInt3): UInt15 => UInt15      = setBits(d, 3, maskFineY, posFineY)
  val flipNametableX: UInt15 => UInt15          = loopy => loopy ^ (0x1 << posNametableX)
  val flipNametableY: UInt15 => UInt15          = loopy => loopy ^ (0x1 << posNametableY)
  val coarseX: UInt15 => UInt5                  = loopy => (loopy & maskCoarseX) >> posCoarseX
  val coarseY: UInt15 => UInt5                  = loopy => (loopy & maskCoarseY) >> posCoarseY
  val nametableX: UInt15 => UInt1               = loopy => (loopy & maskNametableX) >> posNametableX
  val nametableY: UInt15 => UInt1               = loopy => (loopy & maskNametableY) >> posNametableY
  val fineY: UInt15 => UInt3                    = loopy => (loopy & maskFineY) >> posFineY
}

object Mirroring extends Enumeration {
  type Mirroring = Value
  val Vertical, Horizontal, OneScreenLowerBank, OneScreenUpperBank = Value
}

object SpritePriority extends Enumeration {
  type SpritePriority = Value
  val InFrontOfBackground, BehindBackground = Value
}

case class SpriteInfo(
  x: UInt8,
  palette: UInt2,
  priority: SpritePriority,
  spriteLo: UInt8,
  spriteHi: UInt8,
  index: Int
)

case class Rgb(r: Int, g: Int, b: Int)

object Rgb {
  // format: off
  val palette: Vector[Int] = Vector(
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
    // format: on
  ).map { rgb =>
    (rgb.b & 0xff) | ((rgb.g & 0xff) << 8) | ((rgb.r & 0xff) << 16) | (0xff << 24)
  }
}

object Ppu {

  val noOp: NesState => NesState = nes => nes

  private def lift(f: PpuState => PpuState): NesState => NesState =
    NesState.ppuState.modify(f)

  private def liftS(f: PpuState => PpuState): State[NesState, Unit] =
    State.modify(NesState.ppuState.modify(f))

  def setCycle(d: Int, nes: NesState): NesState = {
    nes.ppuState.cycle = d
    nes
  }

  def setScanline(d: Int, nes: NesState): NesState = {
    nes.ppuState.scanline = d
    nes
  }

  def setFrame(d: Long, nes: NesState): NesState = {
    nes.ppuState.frame = d
    nes
  }

  val setVerticalBlank: NesState => NesState =
    lift(VerticalBlank.modify(VerticalBlank.On))

  val clearVerticalBlank: PpuState => PpuState =
    VerticalBlank.modify(VerticalBlank.Off)

  val setSpriteZeroHit: PpuState => PpuState =
    SpriteZeroHit.modify(SpriteZeroHit.On)

  val clearSpriteZeroHit: PpuState => PpuState =
    SpriteZeroHit.modify(SpriteZeroHit.Off)

  val setSpriteOverflow: PpuState => PpuState =
    SpriteOverflow.modify(SpriteOverflow.On)

  val clearSpriteOverflow: PpuState => PpuState =
    SpriteOverflow.modify(SpriteOverflow.Off)

  val clearLoopyW: PpuState => PpuState =
    PpuState.w.set(0x0)

  val incScrollX: NesState => NesState =
    lift { ppu =>
      val coarseX = Loopy.coarseX(ppu.v)
      PpuState.v.modify {
        if (coarseX == 31)
          Loopy.setCoarseX(0) andThen Loopy.flipNametableX
        else
          Loopy.setCoarseX(coarseX + 1)
      }(ppu)
    }

  val incScrollY: NesState => NesState =
    lift { ppu =>
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

  val transferAddressX: NesState => NesState =
    lift { ppu =>
      val tCoarseX    = Loopy.coarseX(ppu.t)
      val tNametableX = Loopy.nametableX(ppu.t)
      PpuState.v.modify {
        Loopy.setCoarseX(tCoarseX) andThen Loopy.setNametableX(tNametableX)
      }(ppu)
    }

  val transferAddressY: NesState => NesState =
    lift { ppu =>
      val tFineY      = Loopy.fineY(ppu.t)
      val tNametableY = Loopy.nametableY(ppu.t)
      val tCoarseY    = Loopy.coarseY(ppu.t)
      PpuState.v.modify {
        Loopy.setFineY(tFineY) andThen Loopy.setNametableY(tNametableY) andThen Loopy.setCoarseY(tCoarseY)
      }(ppu)
    }

  val loadShifters: NesState => NesState =
    lift { ppu =>
      val shifterPatternLo = (ppu.shifterPatternLo & 0xff00) | ppu.nextTileLo
      val shifterPatternHi = (ppu.shifterPatternHi & 0xff00) | ppu.nextTileHi
      val shifterAttrLo    = (ppu.shifterPatternLo & 0xff00) | (if (ppu.nextTileAttr & 0x01) 0xff else 0x00)
      val shifterAttrHi    = (ppu.shifterPatternHi & 0xff00) | (if (ppu.nextTileAttr & 0x02) 0xff else 0x00)
      (PpuState.shifterPatternLo.set(shifterPatternLo) andThen
        PpuState.shifterPatternHi.set(shifterPatternHi) andThen
        PpuState.shifterAttrLo.set(shifterAttrLo) andThen
        PpuState.shifterAttrHi.set(shifterAttrHi))(ppu)
    }

  val updateShifters: NesState => NesState =
    lift(
      PpuState.shifterPatternLo.modify(_ << 1) andThen
        PpuState.shifterPatternHi.modify(_ << 1) andThen
        PpuState.shifterAttrLo.modify(_ << 1) andThen
        PpuState.shifterAttrHi.modify(_ << 1)
    )

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
      val backgroundTableAddress = BackgroundTableAddress.get(ppu).address
      val address                = (backgroundTableAddress << 12) + (ppu.nextTileId << 4) + Loopy.fineY(ppu.v)
      ppuRead(address).runS(nes)
    }

  // TODO: docs
  val fetchHighTileByte: NesState => NesState =
    nes => {
      val ppu                    = nes.ppuState
      val backgroundTableAddress = BackgroundTableAddress.get(ppu).address
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

  def getColor(pixel: Int, palette: Int)(ppu: PpuState): Int = {
    require(palette >= 0 && palette < 8)
    require(pixel >= 0 && pixel < 4)

    val colorAddress =
      if (RenderBackground.get(ppu))
        0x3f00 + (palette << 2) + pixel
      else
        0x0000
    val colorValue = ppu.palettes(mapToPalettesIndex(colorAddress))
    Rgb.palette(colorValue)
  }

  def readPalettes(address: UInt16)(ppu: PpuState): UInt8 = {
    require((address & 0xffff) == address)
    require(address >= 0x3f00 && address < 0x4000, s"failed: $address")

    val addr  = mapToPalettesIndex(address)
    val color = ppu.palettes(addr)
    if (Greyscale.get(ppu)) color & 0x30
    else color
  }

  def writePalettes(address: UInt16, d: UInt8)(ppu: PpuState): PpuState = {
    require((address & 0xffff) == address)
    require(address >= 0x3f00 && address < 0x3fff)

    val addr = mapToPalettesIndex(address)
    PpuState.palettes.modify(_.updated(addr, d))(ppu)
  }

  def cpuRead(address: UInt16): State[NesState, UInt8] = {
    // The PPU exposes eight memory-mapped registers to the CPU, which are mapped to the
    // address range $2000-$2007. Addresses between $2008 and $3FFF are mirrored.
    require(address >= 0x2000 && address <= 0x3fff)
    address & 0x0007 match {
      case 0x0002 => // PPUSTATUS
        readStatus
      case 0x0004 => // OAMDATA
        readOamData
      case 0x0007 => // PPUDATA
        readData
      case _ =>
        State.pure(0x00)
    }
  }

  def cpuWrite(address: UInt16, d: UInt8): NesState => NesState = {
    require(address >= 0x2000 && address <= 0x3fff)
    require((d & 0xff) == d)
    address match {
      case 0x2000 => writeCtrl(d)
      case 0x2001 => writeMask(d)
      case 0x2003 => writeOamAddress(d)
      case 0x2004 => writeOamData(d)
      case 0x2005 => writeScroll(d)
      case 0x2006 => writeAddr(d)
      case 0x2007 => writeData(d)
      case 0x4014 => writeDma(d)
      case _      => throw new RuntimeException(f"Invalid cpu memory write at address $address%#04x")
    }
  }

  def writeCtrl(d: UInt8): NesState => NesState =
    nes => {
      // Set nametables select
      val nametables = NametableAddress.get(d).id
      val t          = (nes.ppuState.t & 0xf3ff) | (nametables << 10)
      lift(PpuState.ctrl.set(d) andThen PpuState.t.set(t))(nes)
    }

  def writeMask(d: UInt8): NesState => NesState =
    lift(PpuState.mask.set(d))

  def writeOamAddress(d: UInt8): NesState => NesState =
    lift(PpuState.oamAddress.set(d))

  def writeOamData(d: UInt8): NesState => NesState =
    nes => {
      val oamAddress = nes.ppuState.oamAddress
      lift(PpuState.oamData.modify(_.updated(oamAddress, d)) andThen PpuState.oamAddress.modify(_ + 1))(nes)
    }

  def writeScroll(d: UInt8): NesState => NesState =
    nes =>
      if (nes.ppuState.w) {
        // Set coarse Y and fine Y
        val t = (nes.ppuState.t & 0x8c1f) | ((d & 0x07) << 12) | ((d & 0xf8) << 2)
        lift(PpuState.t.set(t) andThen PpuState.w.set(0))(nes)
      } else {
        // Set coarse X and fine X
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
        val t = (nes.ppuState.t & 0x80ff) | ((d & 0x3f) << 8)
        lift(PpuState.t.set(t) andThen PpuState.w.set(1))(nes)
      }
    }

  def writeData(d: UInt8): NesState => NesState =
    nes => {
      val delta = AddressIncrementMode.get(nes.ppuState).delta
      (ppuWrite(nes.ppuState.v, d) andThen lift(PpuState.v.modify(_ + delta)))(nes)
    }

  def writeDma(d: UInt8): NesState => NesState =
    nes => {
      val address = d << 8
      (0 until 256).foldLeft(nes) { case (nes, i) =>
        val (nes1, d)  = Cpu.cpuRead(address + i)(nes)
        val oamAddress = nes1.ppuState.oamAddress
        lift(
          PpuState.oamData.modify(_.updated(oamAddress, d)) andThen PpuState.oamAddress.modify(_ + 1)
        )(nes1)
      }
    }

  val readStatus: State[NesState, UInt8] =
    nes => {
      val d    = (nes.ppuState.status & 0xe0) | (nes.ppuState.bufferedData & 0x1f)
      val nes1 = lift(clearLoopyW andThen clearVerticalBlank)(nes)
      (nes1, d)
    }

  val readOamData: State[NesState, UInt8] =
    State.get[NesState].map { nes =>
      val oamAddress = nes.ppuState.oamAddress
      nes.ppuState.oamData(oamAddress)
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
        nes <- liftS(PpuState.bufferedData.set(buffer)).flatMap(_ => State.get)
        delta = AddressIncrementMode.get(nes.ppuState).delta
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
    require((address & 0x3fff) == address, s"address ${hex(address)}")
    if (address >= 0x0000 && address <= 0x1fff)
      Cartridge.ppuWrite(address, d).runS(_) // Patterns
    else if (address >= 0x2000 && address <= 0x3eff)
      lift(writeNametables(address, d))
    else
      lift(writePalettes(address, d))
  }

  def backgroundPixel(ppu: PpuState): Option[(Int, Int)] = {
    val bitMux    = 0x8000 >> ppu.x
    val pixel0    = if (ppu.shifterPatternLo & bitMux) 0x1 else 0x0
    val pixel1    = if (ppu.shifterPatternHi & bitMux) 0x1 else 0x0
    val bgPixel   = (pixel1 << 1) | pixel0
    val palette0  = if (ppu.shifterAttrLo & bitMux) 0x1 else 0x0
    val palette1  = if (ppu.shifterPatternHi & bitMux) 0x1 else 0x0
    val bgPalette = (palette1 << 1) | palette0
    if (bgPixel != 0)
      Option((bgPixel, bgPalette))
    else
      None
  }

  def spritePixel(ppu: PpuState): Option[(Int, SpriteInfo)] =
    ppu.spritesInfo
      .filter { s =>
        val offset = (ppu.cycle - 1) - s.x
        RenderSprites.get(ppu) && offset >= 0 && offset < 8
      }
      .map { s =>
        val bitMux  = 0x8000 >> ((ppu.cycle - 1) - s.x)
        val pixel0  = if (s.spriteLo & bitMux) 0x1 else 0x0
        val pixel1  = if (s.spriteHi & bitMux) 0x1 else 0x0
        val fgPixel = (pixel1 << 1) | pixel0
        (fgPixel, s)
      }
      .find { case (fgPixel, _) => fgPixel != 0 }

  val renderPixel: NesState => NesState =
    nes => {
      val ppu = nes.ppuState
      val x   = ppu.cycle - 1
      val y   = ppu.scanline
      val (pixel, palette, spriteZero) =
        (backgroundPixel(ppu), spritePixel(ppu)) match {
          case (None, None) =>
            (0, 0, false)
          case (None, Some((fgPixel, spriteInfo))) =>
            (fgPixel, spriteInfo.palette, false)
          case (Some((bgPixel, bgPalette)), None) =>
            (bgPixel, bgPalette, false)
          case (Some((bgPixel, bgPalette)), Some((fgPixel, spriteInfo))) =>
            val spriteZero = spriteInfo.index == 0 && x < 255
            if (spriteInfo.priority == SpritePriority.BehindBackground)
              (bgPixel, bgPalette, spriteZero)
            else
              (fgPixel, spriteInfo.palette, spriteZero)
        }

      val color = getColor(pixel, palette)(ppu)
      ppu.canvas.update(y * 2 * 256 + x, color)
      ppu.canvas.update(y * 2 * 256 + x + 1, color)
      ppu.canvas.update((y + 1) * 2 * 256 + x, color)
      ppu.canvas.update((y + 1) * 2 * 256 + x + 1, color)

      if (spriteZero)
        lift(setSpriteZeroHit)(nes)
      else
        nes
    }

  private def flipByte(d: UInt8): UInt8 = {
    val d1 = (d & 0xf0) >> 4 | (d & 0x0f) << 4
    val d2 = (d1 & 0xcc) >> 2 | (d1 & 0x33) << 2
    val d3 = (d2 & 0xaa) >> 1 | (d2 & 0x55) << 1
    d3
  }

  private def fetchSpriteInfo(i: Int, nes: NesState): (NesState, SpriteInfo) = {
    val oamData    = nes.ppuState.oamData
    val y          = oamData(i * 4 + 0)
    val tile       = oamData(i * 4 + 1)
    val attr       = oamData(i * 4 + 2)
    val x          = oamData(i * 4 + 3)
    val row        = nes.ppuState.scanline - y
    val palette    = attr & 0x03 + 0x04 // Sprite palettes are the later 4 in the palette memory
    val flipHor    = attr & 0x40
    val flipVert   = attr & 0x80
    val priority   = SpritePriority((attr >> 5) & 0x1)
    val spriteSize = SpriteSize.get(nes.ppuState.ctrl)
    val table =
      if (spriteSize == SpriteSize.Small)
        SpriteTableAddress.get(nes.ppuState).address
      else
        0x1000 * (tile & 0x1)

    val address =
      if (spriteSize == SpriteSize.Small) {
        // 8x8 mode
        table + tile * 16 + (if (flipVert) row else 7 - row)
      } else if (!flipVert & row < 8) {
        // 8x16 mode, no vertical flip, top half tile
        table + tile * 16 + row
      } else if (!flipVert & row > 8) {
        // 8x16 mode, no vertical flip, bottom half tile
        table + (tile + 1) * 16 + (row - 8)
      } else if (flipVert & (15 - row) < 8) {
        // 8x16 mode, vertical flip, top half tile
        table + tile * 16 + (15 - row)
      } else {
        // 8x16 mode, vertical flip, bottom half tile
        table + (tile + 1) * 16 + (15 - row - 8)
      }

    val (nes1, (spriteLo, spriteHi)) = (
      for {
        lo <- ppuRead(address)
        hi <- ppuRead(address + 8)
      } yield
        if (flipHor)
          (flipByte(lo), flipByte(hi))
        else
          (lo, hi)
    )(nes)

    val spriteInfo = SpriteInfo(x, palette, priority, spriteLo, spriteHi, i)

    (nes1, spriteInfo)
  }

  val evaluateSprites: NesState => NesState =
    nes => {
      val h        = SpriteSize.get(nes.ppuState).height
      val scanline = nes.ppuState.scanline
      val oamData  = nes.ppuState.oamData
      val (nes1, spritesInfo) = (0 until 64)
        .filter { i =>
          val y   = oamData(i * 4 + 0)
          val row = scanline - y
          row >= 0 && row < h
        }
        .foldRight((nes, List.empty[SpriteInfo])) { case (i, (nes, spritesInfo)) =>
          val (nes1, spriteInfo) = fetchSpriteInfo(i, nes)
          (nes1, spriteInfo :: spritesInfo)
        }
      if (spritesInfo.size > 8)
        lift(
          setSpriteOverflow andThen PpuState.spritesInfo.set(spritesInfo.take(8))
        )(nes1)
      else
        lift(PpuState.spritesInfo.set(spritesInfo))(nes1)
    }

  def incCounters(nes: NesState): NesState = {
    val ppu = nes.ppuState
    val n   = ppu.scanline * 341 + ppu.cycle + 1
    val (cycle, scanline, frame) =
      if (n >= 341 * 262) (0, 0, ppu.frame + 1)
      else if (n % 341 == 0) (0, ppu.scanline + 1, ppu.frame)
      else (ppu.cycle + 1, ppu.scanline, ppu.frame)
    val nes1 = setCycle(cycle, nes)
    val nes2 = setScanline(scanline, nes1)
    setFrame(frame, nes2)
  }

  def clock(nes: NesState): NesState = {
    val ppu             = nes.ppuState
    val scanline        = ppu.scanline
    val cycle           = ppu.cycle
    val isVisibleLine   = scanline < 240
    val isPreRenderLine = scanline == 261
    val isRenderLine    = isVisibleLine || isPreRenderLine
    val isRendering     = RenderBackground.get(ppu) || RenderSprites.get(ppu)
    val isPreFetchCycle = cycle >= 321 && cycle <= 336
    val isVisibleCycle  = cycle >= 1 && cycle <= 256
    val isFetchCycle    = isPreFetchCycle || isVisibleCycle

    /*
    // Render a pixel
    val pixel =
      if (isVisibleLine && isVisibleCycle) renderPixel
      else noOp

    // Background logic
    val background =
      if (isRendering && isRenderLine & isFetchCycle)
        cycle % 8 match {
          case 1                 => updateShifters andThen fetchNametableByte
          case 3                 => updateShifters andThen fetchAttributeTableByte
          case 5                 => updateShifters andThen fetchLowTileByte
          case 7                 => updateShifters andThen fetchHighTileByte
          case 0 if cycle == 256 => updateShifters andThen loadShifters andThen incScrollX andThen incScrollY
          case 0                 => updateShifters andThen loadShifters andThen incScrollX
          case _                 => updateShifters
        }
      else if (isRendering && isPreRenderLine && cycle >= 280 && cycle <= 304) transferAddressY
      else if (isRendering && isRenderLine && cycle == 257) transferAddressX
      else noOp

    // Sprite logic
    val sprite =
      if (isRendering && cycle == 257)
        if (isVisibleLine) evaluateSprites
        else lift(PpuState.spritesInfo.set(Seq.empty))
      else noOp
     */

    // vblank logic
    val vblank =
      if (scanline == 241 && cycle == 1)
        setVerticalBlank
      else if (isPreRenderLine && cycle == 1)
        lift(clearVerticalBlank andThen clearSpriteOverflow andThen clearSpriteZeroHit)
      else
        noOp

    // TODO: NMI!!!

//      (lift(incCounters) andThen pixel andThen background andThen sprite andThen vblank)(nes)
    val nes1 = incCounters(nes)
    vblank(nes1)
  }

  def reset(nes: NesState): NesState =
    NesState.ppuState.modify(ppu => PpuState(ppu.mirroring))(nes)
}
