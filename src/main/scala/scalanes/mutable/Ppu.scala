package scalanes.mutable

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
  var spritesInfo: List[SpriteInfo],
  // Other
  val canvas: Array[Int],
  var nmi: Boolean,
  val mirroring: Mirroring
)

object PpuState {
  val nametables: IndexSetter[PpuState, UInt8]        = (i, a, s) => s.nametables = s.nametables.updated(i, a)
  val palettes: IndexSetter[PpuState, UInt8]          = (i, a, s) => s.palettes = s.palettes.updated(i, a)
  val ctrl: Setter[PpuState, UInt8]                   = (a, s) => s.ctrl = a
  val mask: Setter[PpuState, UInt8]                   = (a, s) => s.mask = a
  val status: Setter[PpuState, UInt8]                 = (a, s) => s.status = a
  val bufferedData: Setter[PpuState, UInt8]           = (a, s) => s.bufferedData = a
  val v: Setter[PpuState, UInt15]                     = (a, s) => s.v = a
  val t: Setter[PpuState, UInt15]                     = (a, s) => s.t = a
  val x: Setter[PpuState, UInt3]                      = (a, s) => s.x = a
  val w: Setter[PpuState, UInt1]                      = (a, s) => s.w = a
  val shifterPatternLo: Setter[PpuState, UInt16]      = (a, s) => s.shifterPatternLo = a
  val shifterPatternHi: Setter[PpuState, UInt16]      = (a, s) => s.shifterPatternHi = a
  val shifterAttrLo: Setter[PpuState, UInt16]         = (a, s) => s.shifterAttrLo = a
  val shifterAttrHi: Setter[PpuState, UInt16]         = (a, s) => s.shifterAttrHi = a
  val nextTileId: Setter[PpuState, UInt8]             = (a, s) => s.nextTileId = a
  val nextTileLo: Setter[PpuState, UInt8]             = (a, s) => s.nextTileLo = a
  val nextTileHi: Setter[PpuState, UInt8]             = (a, s) => s.nextTileHi = a
  val nextTileAttr: Setter[PpuState, UInt8]           = (a, s) => s.nextTileAttr = a
  val oamAddress: Setter[PpuState, UInt16]            = (a, s) => s.oamAddress = a
  val oamData: IndexSetter[PpuState, UInt8]           = (i, a, s) => s.oamData = s.oamData.updated(i, a)
  val spritesInfo: Setter[PpuState, List[SpriteInfo]] = (a, s) => s.spritesInfo = a
  val cycle: Setter[PpuState, Int]                    = (a, s) => s.cycle = a
  val scanline: Setter[PpuState, Int]                 = (a, s) => s.scanline = a
  val frame: Setter[PpuState, Long]                   = (a, s) => s.frame = a
  val nmi: Setter[PpuState, Boolean]                  = (a, s) => s.nmi = a

  def flagNametable(ppu: PpuState): UInt3        = (ppu.ctrl & (0x3 << 0)) >> 0
  def flagIncrement(ppu: PpuState): UInt1        = (ppu.ctrl & (0x1 << 2)) >> 2
  def flagSpriteTable(ppu: PpuState): UInt1      = (ppu.ctrl & (0x1 << 3)) >> 3
  def flagBackgroundTable(ppu: PpuState): UInt16 = (ppu.ctrl & (0x1 << 4)) >> 4
  def flagSpriteSize(ppu: PpuState): UInt1       = (ppu.ctrl & (0x1 << 5)) >> 5
  def flagMasterSlave(ppu: PpuState): UInt1      = (ppu.ctrl & (0x1 << 6)) >> 6
  def flagNmi(ppu: PpuState): UInt1              = (ppu.ctrl & (0x1 << 7)) >> 7

  val increments: Map[UInt1, Int]  = Map(0 -> 1, 1 -> 32)
  val spriteSizes: Map[UInt1, Int] = Map(0 -> 8, 1 -> 16)

  def flagGreyscale(ppu: PpuState): UInt1            = (ppu.mask & (0x1 << 0)) >> 0
  def flagRenderBackgroundLeft(ppu: PpuState): UInt1 = (ppu.mask & (0x1 << 1)) >> 1
  def flagRenderSpritesLeft(ppu: PpuState): UInt1    = (ppu.mask & (0x1 << 2)) >> 2
  def flagRenderBackground(ppu: PpuState): UInt1     = (ppu.mask & (0x1 << 3)) >> 3
  def flagRenderSprites(ppu: PpuState): UInt1        = (ppu.mask & (0x1 << 4)) >> 4
  def flagEnhanceRed(ppu: PpuState): UInt1           = (ppu.mask & (0x1 << 5)) >> 5
  def flagEnhanceGreen(ppu: PpuState): UInt1         = (ppu.mask & (0x1 << 6)) >> 6
  def flagEnhanceBlue(ppu: PpuState): UInt1          = (ppu.mask & (0x1 << 7)) >> 7

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
    nmi = false,
    mirroring = mirroring
  )
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
  private val posNametables  = 0x10
  private val maskNametables = 0x3 << posNametables
  private val posFineY       = 12
  private val maskFineY      = 0x7 << posFineY

  def setCoarseX(d: UInt5)(loopy: UInt15): UInt15    = (loopy & ~maskCoarseX) | (d << posCoarseX)
  def setCoarseY(d: UInt5)(loopy: UInt15): UInt15    = (loopy & ~maskCoarseY) | (d << posCoarseY)
  def setNametableX(d: UInt1)(loopy: UInt15): UInt15 = (loopy & ~maskNametableX) | (d << posNametableX)
  def setNametableY(d: UInt1)(loopy: UInt15): UInt15 = (loopy & ~maskNametableY) | (d << posNametableY)
  def setNametables(d: UInt2)(loopy: UInt15): UInt15 = (loopy & ~maskNametables) | (d << posNametables)
  def setFineY(d: UInt3)(loopy: UInt15): UInt15      = (loopy & ~maskFineY) | (d << posFineY)
  val flipNametableX: UInt15 => UInt15               = loopy => loopy ^ (0x1 << posNametableX)
  val flipNametableY: UInt15 => UInt15               = loopy => loopy ^ (0x1 << posNametableY)
  val coarseX: UInt15 => UInt5                       = loopy => (loopy & maskCoarseX) >> posCoarseX
  val coarseY: UInt15 => UInt5                       = loopy => (loopy & maskCoarseY) >> posCoarseY
  val nametableX: UInt15 => UInt1                    = loopy => (loopy & maskNametableX) >> posNametableX
  val nametableY: UInt15 => UInt1                    = loopy => (loopy & maskNametableY) >> posNametableY
  val fineY: UInt15 => UInt3                         = loopy => (loopy & maskFineY) >> posFineY
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
    nes => {
      val ppu = f(nes.ppuState)
      NesState.ppuState.set(ppu)(nes)
    }

  private def liftS(f: PpuState => PpuState): State[NesState, Unit] =
    State.modify(lift(f))

  val setVerticalBlank: PpuState => PpuState =
    ppu => PpuState.status.set(ppu.status | (0x1 << 7))(ppu)

  val clearVerticalBlank: PpuState => PpuState =
    ppu => PpuState.status.set(ppu.status & ~(0x1 << 7))(ppu)

  val setSpriteZeroHit: PpuState => PpuState =
    ppu => PpuState.status.set(ppu.status | (0x1 << 6))(ppu)

  val clearSpriteZeroHit: PpuState => PpuState =
    ppu => PpuState.status.set(ppu.status & ~(0x1 << 6))(ppu)

  val setSpriteOverflow: PpuState => PpuState =
    ppu => PpuState.status.set(ppu.status | (0x1 << 5))(ppu)

  val clearSpriteOverflow: PpuState => PpuState =
    ppu => PpuState.status.set(ppu.status & ~(0x1 << 5))(ppu)

  val clearLoopyW: PpuState => PpuState =
    PpuState.w.set(0x0)

  val setNmi: PpuState => PpuState =
    ppu => PpuState.nmi.set(true)(ppu)

  val clearNmi: PpuState => PpuState =
    ppu => PpuState.nmi.set(false)(ppu)

  val incScrollX: NesState => NesState =
    nes => {
      val ppu     = nes.ppuState
      val coarseX = Loopy.coarseX(ppu.v)
      val v1 =
        if (coarseX == 31) {
          val v1 = Loopy.setCoarseX(0)(ppu.v)
          Loopy.flipNametableX(v1)
        } else
          Loopy.setCoarseX(coarseX + 1)(ppu.v)
      val ppu1 = PpuState.v.set(v1)(ppu)
      NesState.ppuState.set(ppu1)(nes)
    }

  val incScrollY: NesState => NesState =
    nes => {
      val ppu     = nes.ppuState
      val coarseY = Loopy.coarseY(ppu.v)
      val fineY   = Loopy.fineY(ppu.v)
      val v1 =
        if (fineY < 7)
          Loopy.setFineY(fineY + 1)(ppu.v)
        else if (coarseY == 29) {
          val v1 = Loopy.setFineY(0)(ppu.v)
          val v2 = Loopy.setCoarseY(0)(v1)
          Loopy.flipNametableY(v2)
        } else if (coarseY == 31) {
          val v1 = Loopy.setFineY(0)(ppu.v)
          Loopy.setCoarseY(0)(v1)
        } else {
          val v1 = Loopy.setFineY(0)(ppu.v)
          Loopy.setCoarseY(coarseY + 1)(v1)
        }
      val ppu1 = PpuState.v.set(v1)(ppu)
      NesState.ppuState.set(ppu1)(nes)
    }

  val transferAddressX: NesState => NesState =
    nes => {
      val ppu         = nes.ppuState
      val tCoarseX    = Loopy.coarseX(ppu.t)
      val tNametableX = Loopy.nametableX(ppu.t)
      val v1          = Loopy.setCoarseX(tCoarseX)(ppu.v)
      val v2          = Loopy.setNametableX(tNametableX)(v1)
      val ppu1        = PpuState.v.set(v2)(ppu)
      NesState.ppuState.set(ppu1)(nes)
    }

  val transferAddressY: NesState => NesState =
    nes => {
      val ppu         = nes.ppuState
      val tFineY      = Loopy.fineY(ppu.t)
      val tNametableY = Loopy.nametableY(ppu.t)
      val tCoarseY    = Loopy.coarseY(ppu.t)
      val v1          = Loopy.setFineY(tFineY)(ppu.v)
      val v2          = Loopy.setNametableY(tNametableY)(v1)
      val v3          = Loopy.setCoarseY(tCoarseY)(v2)
      val ppu1        = PpuState.v.set(v3)(ppu)
      NesState.ppuState.set(ppu1)(nes)
    }

  val loadShifters: NesState => NesState =
    nes => {
      val ppu              = nes.ppuState
      val shifterPatternLo = (ppu.shifterPatternLo & 0xff00) | ppu.nextTileLo
      val shifterPatternHi = (ppu.shifterPatternHi & 0xff00) | ppu.nextTileHi
      val shifterAttrLo    = (ppu.shifterAttrLo & 0xff00) | (if (ppu.nextTileAttr & 0x01) 0xff else 0x00)
      val shifterAttrHi    = (ppu.shifterAttrHi & 0xff00) | (if (ppu.nextTileAttr & 0x02) 0xff else 0x00)
      val ppu1             = PpuState.shifterPatternLo.set(shifterPatternLo)(ppu)
      val ppu2             = PpuState.shifterPatternHi.set(shifterPatternHi)(ppu1)
      val ppu3             = PpuState.shifterAttrLo.set(shifterAttrLo)(ppu2)
      val ppu4             = PpuState.shifterAttrHi.set(shifterAttrHi)(ppu3)
      NesState.ppuState.set(ppu4)(nes)
    }

  val updateShifters: NesState => NesState =
    nes => {
      val ppu              = nes.ppuState
      val shifterPatternLo = ppu.shifterPatternLo << 1
      val shifterPatternHi = ppu.shifterPatternHi << 1
      val shifterAttrLo    = ppu.shifterAttrLo << 1
      val shifterAttrHi    = ppu.shifterAttrHi << 1
      val ppu1             = PpuState.shifterPatternLo.set(shifterPatternLo)(ppu)
      val ppu2             = PpuState.shifterPatternHi.set(shifterPatternHi)(ppu1)
      val ppu3             = PpuState.shifterAttrLo.set(shifterAttrLo)(ppu2)
      val ppu4             = PpuState.shifterAttrHi.set(shifterAttrHi)(ppu3)
      NesState.ppuState.set(ppu4)(nes)
    }

  // TODO: docs
  val fetchNametableByte: NesState => NesState =
    nes => {
      val address   = 0x2000 | (nes.ppuState.v & 0x0fff)
      val (nes1, d) = ppuRead(address)(nes)
      val ppu1      = PpuState.nextTileId.set(d)(nes1.ppuState)
      NesState.ppuState.set(ppu1)(nes1)
    }

  // TODO: docs
  val fetchAttributeTableByte: NesState => NesState =
    nes => {
      val v         = nes.ppuState.v
      val address   = 0x23c0 | (v & 0x0c00) | ((v >> 4) & 0x38) | ((v >> 2) & 0x07)
      val shift     = ((v >> 4) & 4) | (v & 2)
      val (nes1, d) = ppuRead(address)(nes)
      val attr      = (d >> shift) & 0x3
      val ppu1      = PpuState.nextTileAttr.set(attr)(nes1.ppuState)
      NesState.ppuState.set(ppu1)(nes1)
    }

  // TODO: docs
  val fetchLowTileByte: NesState => NesState =
    nes => {
      val ppu                    = nes.ppuState
      val backgroundTableAddress = PpuState.flagBackgroundTable(ppu)
      val address                = (backgroundTableAddress << 12) + (ppu.nextTileId << 4) + Loopy.fineY(ppu.v)
      val (nes1, d)              = ppuRead(address).run(nes)
      val ppu1                   = PpuState.nextTileLo.set(d)(nes1.ppuState)
      NesState.ppuState.set(ppu1)(nes1)
    }

  // TODO: docs
  val fetchHighTileByte: NesState => NesState =
    nes => {
      val ppu                    = nes.ppuState
      val backgroundTableAddress = PpuState.flagBackgroundTable(ppu)
      val address                = (backgroundTableAddress << 12) + (ppu.nextTileId << 4) + Loopy.fineY(ppu.v) + 8
      val (nes1, d)              = ppuRead(address).run(nes)
      val ppu1                   = PpuState.nextTileHi.set(d)(nes1.ppuState)
      NesState.ppuState.set(ppu1)(nes1)
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
    PpuState.nametables.set(i, d)(ppu)
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
      if (PpuState.flagRenderBackground(ppu))
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
    if (PpuState.flagGreyscale(ppu)) color & 0x30
    else color
  }

  def writePalettes(address: UInt16, d: UInt8)(ppu: PpuState): PpuState = {
    require((address & 0xffff) == address)
    require(address >= 0x3f00 && address < 0x3fff)

    val addr = mapToPalettesIndex(address)
    PpuState.palettes.set(addr, d)(ppu)
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
    require((address >= 0x2000 && address <= 0x3fff) || address == 0x4014, f"Invalid address $address%#04x")
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
      val ppu        = PpuState.ctrl.set(d)(nes.ppuState)
      val nametables = PpuState.flagNametable(ppu)
      val t          = (ppu.t & 0xf3ff) | (nametables << 10)
      val ppu1       = PpuState.t.set(t)(ppu)
      NesState.ppuState.set(ppu1)(nes)
    }

  def writeMask(d: UInt8): NesState => NesState =
    lift(PpuState.mask.set(d))

  def writeOamAddress(d: UInt8): NesState => NesState =
    lift(PpuState.oamAddress.set(d))

  def writeOamData(d: UInt8): NesState => NesState =
    nes => {
      val oamAddress = nes.ppuState.oamAddress
      val ppu1       = PpuState.oamData.set(oamAddress, d)(nes.ppuState)
      val ppu2       = PpuState.oamAddress.set(oamAddress + 1)(ppu1)
      NesState.ppuState.set(ppu2)(nes)
    }

  def writeScroll(d: UInt8): NesState => NesState =
    nes => {
      val ppu = nes.ppuState
      val ppu1 = if (ppu.w) {
        // Set coarse Y and fine Y
        val t    = (ppu.t & 0x8c1f) | ((d & 0x07) << 12) | ((d & 0xf8) << 2)
        val ppu1 = PpuState.t.set(t)(ppu)
        PpuState.w.set(0)(ppu1)
      } else {
        // Set coarse X and fine X
        val t    = (ppu.t & 0xffe0) | (d >> 3)
        val x    = d & 0x7
        val ppu1 = PpuState.t.set(t)(ppu)
        val ppu2 = PpuState.x.set(x)(ppu1)
        PpuState.w.set(1)(ppu2)
      }
      NesState.ppuState.set(ppu1)(nes)
    }

  def writeAddr(d: UInt8): NesState => NesState =
    nes => {
      val ppu = nes.ppuState
      val ppu1 = if (ppu.w) {
        // Upper address byte
        val t    = (ppu.t & 0xff00) | d
        val ppu1 = PpuState.t.set(t)(ppu)
        val ppu2 = PpuState.v.set(t)(ppu1)
        PpuState.w.set(0)(ppu2)
      } else {
        // Lower address byte
        val t    = (ppu.t & 0x80ff) | ((d & 0x3f) << 8)
        val ppu1 = PpuState.t.set(t)(ppu)
        PpuState.w.set(1)(ppu1)
      }
      NesState.ppuState.set(ppu1)(nes)
    }

  def writeData(d: UInt8): NesState => NesState =
    nes => {
      val ppu   = nes.ppuState
      val delta = PpuState.increments(PpuState.flagIncrement(ppu))
      val nes1  = ppuWrite(ppu.v, d)(nes)
      val ppu1  = PpuState.v.set(ppu.v + delta)(ppu)
      NesState.ppuState.set(ppu1)(nes1)
    }

  def writeDma(d: UInt8): NesState => NesState =
    nes => {
      val address = d << 8
      (0 until 256).foldLeft(nes) { case (nes, i) =>
        val (nes1, d) = Cpu.cpuRead(address + i)(nes)
        writeOamData(d)(nes1)
      }
    }

  val readStatus: State[NesState, UInt8] =
    nes => {
      val d    = (nes.ppuState.status & 0xe0) | (nes.ppuState.bufferedData & 0x1f)
      val ppu1 = clearLoopyW(nes.ppuState)
      val ppu2 = clearVerticalBlank(ppu1)
      val ppu3 = clearNmi(ppu2)
      val nes1 = NesState.ppuState.set(ppu3)(nes)
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
        delta = PpuState.increments(PpuState.flagIncrement(nes.ppuState))
        v     = nes.ppuState.v
        _ <- liftS(PpuState.v.set(v + delta))
      } yield d
    }

  def ppuRead(address: UInt16): State[NesState, UInt8] = {
    require((address & 0x3fff) == address, f"Invalid address $address%#04x")

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
      Cartridge.ppuWrite(address, d) // Patterns
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
    val palette1  = if (ppu.shifterAttrHi & bitMux) 0x1 else 0x0
    val bgPalette = (palette1 << 1) | palette0
    if (bgPixel != 0)
      Option((bgPixel, bgPalette))
    else
      None
  }

  def spritePixel(ppu: PpuState): Option[(Int, SpriteInfo)] = {
    val isRenderSprites = PpuState.flagRenderSprites(ppu)

    def isRenderSprite(s: SpriteInfo): Boolean = {
      val offset = (ppu.cycle - 1) - s.x
      isRenderSprites && offset >= 0 && offset < 8
    }

    @tailrec
    def helper(sprites: List[SpriteInfo]): Option[(Int, SpriteInfo)] =
      sprites match {
        case Nil                             => None
        case s :: tail if !isRenderSprite(s) => helper(tail)
        case s :: tail =>
          val offset  = (ppu.cycle - 1) - s.x
          val bitMux  = 0x80 >> offset
          val pixel0  = if (s.spriteLo & bitMux) 0x1 else 0x0
          val pixel1  = if (s.spriteHi & bitMux) 0x1 else 0x0
          val fgPixel = (pixel1 << 1) | pixel0
          if (fgPixel != 0)
            Option((fgPixel, s))
          else
            helper(tail)
      }
    helper(ppu.spritesInfo)
  }

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
      ppu.canvas.update(2 * y * 2 * 256 + 2 * x, color)
      ppu.canvas.update(2 * y * 2 * 256 + 2 * x + 1, color)
      ppu.canvas.update((2 * y + 1) * 2 * 256 + 2 * x, color)
      ppu.canvas.update((2 * y + 1) * 2 * 256 + 2 * x + 1, color)

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
    val palette    = (attr & 0x03) + 0x04 // Sprite palettes are the later 4 in the palette memory
    val flipHor    = attr & 0x40
    val flipVert   = attr & 0x80
    val priority   = SpritePriority((attr >> 5) & 0x1)
    val spriteSize = PpuState.flagSpriteSize(nes.ppuState)
    val table =
      if (spriteSize == 0)
        PpuState.flagSpriteTable(nes.ppuState) << 12
      else
        0x1000 * (tile & 0x1)

    val address =
      if (spriteSize == 0 && !flipVert) {
        // 8x8 mode, no vertical flip
        table + tile * 16 + row
      } else if (spriteSize == 0) {
        // 8x8 mode, vertical flip
        table + tile * 16 + (7 - row)
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
      val h        = PpuState.spriteSizes(PpuState.flagSpriteSize(nes.ppuState))
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
      if (spritesInfo.size > 8) {
        val ppu1 = PpuState.spritesInfo.set(spritesInfo.take(8))(nes1.ppuState)
        val ppu2 = setSpriteOverflow(ppu1)
        NesState.ppuState.set(ppu2)(nes1)
      } else {
        val ppu1 = PpuState.spritesInfo.set(spritesInfo)(nes1.ppuState)
        NesState.ppuState.set(ppu1)(nes1)
      }
    }

  def incCounters(nes: NesState): NesState = {
    def setCounters(cycle: Int, scanline: Int, frame: Long)(ppu: PpuState): PpuState = {
      val ppu1 = PpuState.cycle.set(cycle)(ppu)
      val ppu2 = PpuState.scanline.set(scanline)(ppu1)
      val ppu3 = PpuState.frame.set(frame)(ppu2)
      ppu3
    }
    val ppu = nes.ppuState
    val n   = ppu.scanline * 341 + ppu.cycle + 1
    val ppu1 =
      if (n >= 341 * 262)
        setCounters(0, 0, ppu.frame + 1)(ppu)
      else if (n % 341 == 0)
        setCounters(0, ppu.scanline + 1, ppu.frame)(ppu)
      else
        setCounters(ppu.cycle + 1, ppu.scanline, ppu.frame)(ppu)
    NesState.ppuState.set(ppu1)(nes)
  }

  def clock(nes: NesState): NesState = {
    val ppu             = nes.ppuState
    val scanline        = ppu.scanline
    val cycle           = ppu.cycle
    val isVisibleLine   = scanline < 240
    val isPreRenderLine = scanline == 261
    val isRenderLine    = isVisibleLine || isPreRenderLine
    val isRendering     = PpuState.flagRenderBackground(ppu) || PpuState.flagRenderSprites(ppu)
    val isPreFetchCycle = cycle >= 321 && cycle <= 336
    val isVisibleCycle  = cycle >= 1 && cycle <= 256
    val isFetchCycle    = isPreFetchCycle || isVisibleCycle
    val nmiEnabled      = PpuState.flagNmi(ppu)

    // Render a pixel
    val nes1 =
      if (isVisibleLine && isVisibleCycle) renderPixel(nes)
      else nes

    // Background logic
    val nes2 =
      if (isRendering && isRenderLine & isFetchCycle)
        cycle % 8 match {
          case 1                 => fetchNametableByte(updateShifters(nes1))
          case 3                 => fetchAttributeTableByte(updateShifters(nes1))
          case 5                 => fetchLowTileByte(updateShifters(nes1))
          case 7                 => fetchHighTileByte(updateShifters(nes1))
          case 0 if cycle == 256 => incScrollX(incScrollY(loadShifters(updateShifters(nes1))))
          case 0                 => incScrollX(loadShifters(updateShifters(nes1)))
          case _                 => updateShifters(nes1)
        }
      else if (isRendering && isPreRenderLine && cycle >= 280 && cycle <= 304) transferAddressY(nes1)
      else if (isRendering && isRenderLine && cycle == 257) transferAddressX(nes1)
      else nes1

    // Sprite logic
    val nes3 =
      if (isRendering && cycle == 257)
        if (isVisibleLine) evaluateSprites(nes2)
        else lift(PpuState.spritesInfo.set(List.empty))(nes2)
      else nes2

    // vblank logic
    val ppu1 =
      if (scanline == 241 && cycle == 1 && nmiEnabled)
        setVerticalBlank(setNmi(nes3.ppuState))
      else if (scanline == 241 && cycle == 1)
        setVerticalBlank(nes3.ppuState)
      else if (isPreRenderLine && cycle == 1)
        clearVerticalBlank(clearSpriteOverflow(clearSpriteZeroHit(clearNmi(nes3.ppuState))))
      else
        nes3.ppuState
    val nes4 = NesState.ppuState.set(ppu1)(nes3)

    incCounters(nes4)
  }

  def reset(nes: NesState): NesState = {
    val ppu = PpuState(nes.ppuState.mirroring)
    NesState.ppuState.set(ppu)(nes)
  }
}
