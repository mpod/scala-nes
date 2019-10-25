package scalanes

import cats.Monad
import cats.data.State
import monocle.Lens
import monocle.macros.GenLens
import scalanes.AddressIncrementMode.AddressIncrementMode
import scalanes.BackgroundTableAddress.BackgroundTableAddress
import scalanes.MasterSlaveMode.MasterSlaveMode
import scalanes.Mirroring.Mirroring
import scalanes.NametableAddress.NametableAddress
import scalanes.NmiMode.NmiMode
import scalanes.SpriteSize.SpriteSize
import scalanes.SpriteTableAddress.SpriteTableAddress

import scala.language.implicitConversions

case class PpuState(patterns: Vector[Vector[UInt8]],
                    nametables: Vector[UInt8],
                    palettes: Vector[UInt8],
                    registers: PpuRegisters,
                    mirroring: Mirroring,
                    scanline: Int,
                    cycle: Int,
                    bgRenderingState: BgRenderingState,
                    pixels: Vector[Int])

object PpuState {
  val nametables: Lens[PpuState, Vector[UInt8]] = GenLens[PpuState](_.nametables)
  val palettes: Lens[PpuState, Vector[UInt8]] = GenLens[PpuState](_.palettes)
  val registers: Lens[PpuState, PpuRegisters] = GenLens[PpuState](_.registers)
  val mirroring: Lens[PpuState, Mirroring] = GenLens[PpuState](_.mirroring)
  val scanline: Lens[PpuState, Int] = GenLens[PpuState](_.scanline)
  val cycle: Lens[PpuState, Int] = GenLens[PpuState](_.cycle)
  val bgRenderingState: Lens[PpuState, BgRenderingState] = GenLens[PpuState](_.bgRenderingState)
}

case class PpuRegisters(ctrl: PpuCtrl, mask: PpuMask, status: PpuStatus, data: UInt8, loopy: LoopyRegisters)

object PpuRegisters {
  val ctrl: Lens[PpuRegisters, PpuCtrl] = GenLens[PpuRegisters](_.ctrl)
  val mask: Lens[PpuRegisters, PpuMask] = GenLens[PpuRegisters](_.mask)
  val status: Lens[PpuRegisters, PpuStatus] = GenLens[PpuRegisters](_.status)
  val data: Lens[PpuRegisters, UInt8] = GenLens[PpuRegisters](_.data)
  val loopy: Lens[PpuRegisters, LoopyRegisters] = GenLens[PpuRegisters](_.loopy)

  def empty: PpuRegisters = ???
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
}

object NametableAddress extends Enumeration {
  type NametableAddress = Val
  protected case class Val(address: UInt16) extends super.Val
  implicit def valueToVal(x: Value): Val = x.asInstanceOf[Val]
  val First = Val(0x2000)
  val Second = Val(0x2400)
  val Third = Val(0x2800)
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
  type SpriteSize = Value
  val Small, Large = Value
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
    new PpuMask(d & 0x01, (d >> 1) & 0x01, (d >> 2) & 0x01, (d >> 3) & 0x01,
      (d >> 4) & 0x01, (d >> 5) & 0x01, (d >> 6) & 0x01, (d >> 7) & 0x01)
  }
}

case class PpuStatus(spriteOverflow: Boolean, spriteZeroHit: Boolean, verticalBlank: Boolean) {
  def asUInt8: UInt8 = ???
}

object PpuStatus {
  val spriteOverflow: Lens[PpuStatus, Boolean] = GenLens[PpuStatus](_.spriteOverflow)
  val spriteZeroHit: Lens[PpuStatus, Boolean]  = GenLens[PpuStatus](_.spriteZeroHit)
  val verticalBlank: Lens[PpuStatus, Boolean]  = GenLens[PpuStatus](_.verticalBlank)

  def apply(a: UInt8): PpuStatus = new PpuStatus(a & 0x20, a & 0x40, a & 0x80)
}

case class LoopyRegisters(v: LoopyAddress, t: LoopyAddress, x: UInt3, w: Boolean) {
  require((x & 0x7) == x)
}

object LoopyRegisters {
  val v: Lens[LoopyRegisters, LoopyAddress] = GenLens[LoopyRegisters](_.v)
  val t: Lens[LoopyRegisters, LoopyAddress] = GenLens[LoopyRegisters](_.t)
  val x: Lens[LoopyRegisters, UInt3] = GenLens[LoopyRegisters](_.x)
  val w: Lens[LoopyRegisters, Boolean] = GenLens[LoopyRegisters](_.w)
}

case class LoopyAddress(coarseX: UInt5, coarseY: UInt5, nametables: UInt2, fineY: UInt3) {
  require((coarseX & 0x1F) == coarseX)
  require((coarseY & 0x1F) == coarseY)
  require((nametables & 0x3) == nametables)
  require((fineY & 0x7) == fineY)

  def setCoarseX(d: UInt5): LoopyAddress = LoopyAddress.coarseX.set(d)(this)
  def incCoarseX(): LoopyAddress = setCoarseX(coarseX + 1)
  def setCoarseY(d: UInt5): LoopyAddress = LoopyAddress.coarseY.set(d)(this)
  def setNametable(d: UInt2): LoopyAddress = LoopyAddress.nametable.set(d)(this)
  def setFineY(d: UInt3): LoopyAddress = LoopyAddress.fineY.set(d)(this)
  def setNametableX(d: UInt1): LoopyAddress = LoopyAddress.nametable.modify(_ | (d & 0x1))(this)
  def setNametableY(d: UInt1): LoopyAddress = LoopyAddress.nametable.modify(_ | ((d & 0x1) << 1))(this)
  def flipNametableX(): LoopyAddress = setNametable(nametables ^ 0x1)
  def flipNametableY(): LoopyAddress = setNametable(nametables ^ 0x2)
  def nametableX: UInt1 = nametables & 0x1
  def nametableY: UInt1 = (nametables & 0x2) >> 1

  def asUInt16: UInt16 = ((fineY & 0x7) << 12) | ((nametables & 0x3) << 10) | ((coarseY & 0x1F) << 5) | (coarseX & 0x1F)
}

object LoopyAddress {
  val coarseX: Lens[LoopyAddress, UInt5] = GenLens[LoopyAddress](_.coarseX)
  val coarseY: Lens[LoopyAddress, UInt5] = GenLens[LoopyAddress](_.coarseY)
  val nametable: Lens[LoopyAddress, UInt2] = GenLens[LoopyAddress](_.nametables)
  val fineY: Lens[LoopyAddress, UInt3] = GenLens[LoopyAddress](_.fineY)

  def apply(address: UInt16): LoopyAddress = new LoopyAddress(
    address & 0x1F,
    (address >> 5) & 0x1F,
    (address >> 10) & 0x3,
    (address >> 12) & 0x7
  )
}

case class BgRenderingState(patternShiftLo: UInt16,
                            patternShiftHi: UInt16,
                            attributeShiftLo: UInt8,
                            attributeShiftHi: UInt8,
                            nextTileId: UInt16,
                            nextTileAttribute: UInt16,
                            nextTileLsb: UInt16,
                            nextTileMsb: UInt16) {
  def updateShifters(): BgRenderingState =
    copy(
      patternShiftLo = patternShiftLo << 1,
      patternShiftHi = patternShiftHi << 1,
      attributeShiftLo = attributeShiftLo << 1,
      attributeShiftHi = attributeShiftHi << 1
    )
}

object Mirroring extends Enumeration {
  type Mirroring = Value
  val Vertical, Horizontal, OneScreenLowerBank, OneScreenUpperBank = Value
}

object Ppu {

  private val ctrlRegister = PpuState.registers composeLens PpuRegisters.ctrl
  private val statusRegister = PpuState.registers composeLens PpuRegisters.status
  private val maskRegister = PpuState.registers composeLens PpuRegisters.mask
  private val loopyRegisters = PpuState.registers composeLens PpuRegisters.loopy

  implicit class PpuStateOps[A](val a: State[PpuState, A]) extends AnyVal {
    def toNesState: State[NesState, A] = a.transformS(
      NesState.ppuState.get,
      (nesState, ppuState) => NesState.ppuState.set(ppuState)(nesState)
    )
  }

  def setVerticalBlank(d: Boolean): State[PpuState, Unit] =
    State.modify((statusRegister composeLens PpuStatus.verticalBlank).set(d))

  def getData: State[PpuState, UInt8] = State.inspect(PpuState.registers.get).map(_.data)

  def setData(d: UInt8): State[PpuState, Unit] =
    State.modify((PpuState.registers composeLens PpuRegisters.data).set(d))

  def getStatus: State[PpuState, PpuStatus] = State.inspect(statusRegister.get)

  def setStatus(d: PpuStatus): State[PpuState, Unit] =
    State.set(statusRegister.set(d))

  def getCtrl: State[PpuState, PpuCtrl] = State.inspect(ctrlRegister.get)

  def setCtrl(d: UInt8): State[PpuState, Unit] =
    State.modify(ctrlRegister.set(PpuCtrl(d)))

  def getMask: State[PpuState, PpuMask] =
    State.inspect(maskRegister.get)

  def setMask(d: UInt8): State[PpuState, Unit] =
    State.modify(maskRegister.set(PpuMask(d)))

  def clearLoopyW: State[PpuState, Unit] =
    State.modify((loopyRegisters composeLens LoopyRegisters.w).set(false))

  def getLoopyV: State[PpuState, LoopyAddress] =
    State.inspect((loopyRegisters composeLens LoopyRegisters.v).get)

  def setLoopyV(d: LoopyAddress): State[PpuState, Unit] =
    State.modify((loopyRegisters composeLens LoopyRegisters.v).set(d))

  def getLoopyT: State[PpuState, LoopyAddress] =
    State.inspect((loopyRegisters composeLens LoopyRegisters.t).get)

  def setLoopyT(d: LoopyAddress): State[PpuState, Unit] =
    State.modify((loopyRegisters composeLens LoopyRegisters.t).set(d))

  def getLoopyX: State[PpuState, UInt3] =
    State.inspect(loopyRegisters.get).map(_.x)

  def setLoopyX(d: UInt3): State[PpuState, Unit] =
    State.modify((loopyRegisters composeLens LoopyRegisters.x).set(d))

  def getLoopyW: State[PpuState, Boolean] =
    State.inspect(loopyRegisters.get).map(_.w)

  def setLoopyW(d: Boolean): State[PpuState, Unit] =
    State.modify((loopyRegisters composeLens LoopyRegisters.w).set(d))

  def isRendering: State[PpuState, Boolean] =
    getMask.map(m => m.renderBackground || m.renderSprites)

  def incScanline: State[PpuState, Unit] =
    State.modify(PpuState.scanline.modify(_ + 1))

  def resetScanline: State[PpuState, Unit] =
    State.set(PpuState.scanline.set(0))

  def getScanline: State[PpuState, Int] =
    State.inspect(PpuState.scanline.get)

  def incCycle: State[PpuState, Unit] =
    State.modify(PpuState.cycle.modify(_ + 1))

  def resetCycle: State[PpuState, Unit] =
    State.set(PpuState.cycle.set(0))

  def getCycle: State[PpuState, Int] =
    State.inspect(PpuState.cycle.get)

  def getMirroring: State[PpuState, Mirroring] =
    State.inspect(PpuState.mirroring.get)

  def getBgRenderingState: State[PpuState, BgRenderingState] =
    State.inspect(PpuState.bgRenderingState.get)

  def setBgRenderingState(d: BgRenderingState): State[PpuState, Unit] =
    State.modify(PpuState.bgRenderingState.set)

  def incScrollX: State[PpuState, Unit] =
    Monad[State[PpuState, *]].ifM(isRendering)(
      ifTrue = getLoopyV.flatMap { v =>
        if (v.coarseX == 31) setLoopyV(v.setCoarseX(0).flipNametableX())
        else setLoopyV(v)
      },
      ifFalse = State.pure()
    )

  def incScrollY: State[PpuState, Unit] =
    Monad[State[PpuState, *]].ifM(isRendering)(
      ifTrue = getLoopyV.flatMap { v =>
        val updated = if (v.fineY < 7) v.setFineY(v.fineY + 1)
        else if (v.coarseY == 29) v.setFineY(0).setCoarseY(0).flipNametableY()
        else if (v.coarseY == 31) v.setFineY(0).setCoarseY(0)
        else v.setCoarseY(v.coarseY + 1)
        setLoopyV(updated)
      },
      ifFalse = State.pure()
    )

  def transferAddressX: State[PpuState, Unit] =
    Monad[State[PpuState, *]].ifM(isRendering)(
      ifTrue = for {
        v <- getLoopyV
        t <- getLoopyT
        updatedV = v.setCoarseX(t.coarseX).setNametableX(t.nametableX)
        _ <- setLoopyV(updatedV)
      } yield (),
      ifFalse = State.pure()
    )

  def transferAddressY: State[PpuState, Unit] =
    Monad[State[PpuState, *]].ifM(isRendering)(
      ifTrue = for {
        v <- getLoopyV
        t <- getLoopyT
        updatedV = v.setFineY(t.fineY).setNametableY(t.nametableY).setCoarseY(t.coarseY)
        _ <- setLoopyV(updatedV)
      } yield (),
      ifFalse = State.pure()
    )

  def loadBgShifters: State[PpuState, Unit] = ???

  def updateShifters(): State[PpuState, Unit] = ???

  private def mapToNametableAddress(address: UInt16, mirroring: Mirroring): UInt16 = {
    val addr = address & 0x0FFF
    if (mirroring == Mirroring.Vertical) addr & 0x07FF
    else if (mirroring == Mirroring.Horizontal) ((addr & 0x800) >> 1) | (addr & 0x3FF)
    else addr
  }

  def readNametables(address: UInt16): State[PpuState, UInt8] = {
    require((address & 0xFFFF) == address)
    require(address >= 0x2000 && address < 0x3F00)
    for {
      mirroring <- getMirroring
      addr = mapToNametableAddress(address, mirroring)
      d <- State.inspect(PpuState.nametables.get).map(_.apply(addr))
    } yield d
  }

  def writeNametables(address: UInt16, d: UInt8): State[PpuState, Unit] = {
    require((address & 0xFFFF) == address)
    require(address >= 0x2000 && address < 0x3F00)
    for {
      mirroring <- getMirroring
      addr = mapToNametableAddress(address, mirroring)
      _ <- State.modify(PpuState.nametables.modify(_.updated(addr, d)))
    } yield Unit
  }

  private def mapToPalettesIndex(address: UInt16): UInt16 = {
    val addr = address & 0x1F
    // Mirror $3F10, $3F14, $3F18, $3F1C to $3F00, $3F04, $3F08, $3F0C
    if ((addr & 0x13) == 0x10) addr & ~0x10 else addr
  }

  def getColor(address: UInt16): State[PpuState, UInt8] =
    State.inspect(PpuState.palettes.get).map(_.apply(mapToPalettesIndex(address)))

  def setColor(address: UInt16, d: UInt8): State[PpuState, Unit] =
    State.modify(PpuState.palettes.modify(_.updated(mapToPalettesIndex(address), d)))

  def cpuRead(address: UInt16): State[NesState, UInt8] = {
    // The PPU exposes eight memory-mapped registers to the CPU, which are mapped to the
    // address range $2000-$2007. Addresses between $2008 and $3FFF are mirrored.
    require(address >= 0x2000 && address <= 0x3FFF)
    val address0 = address & 0x7
    val zero = State.pure[NesState, UInt8](0x00)
    if (address0 == 0x0000)      zero // PPUCTRL
    else if (address0 == 0x0001) zero // PPUMASK
    else if (address0 == 0x0002)      // PPUSTATUS
      (
        for {
          data   <- getData
          status <- getStatus.map(_.asUInt8)
          _      <- clearLoopyW
          _      <- setVerticalBlank(false)
        } yield (status & 0xE0) | (data & 0x1F)
      ).toNesState
    else if (address0 == 0x0003) zero // OAMADDR
    else if (address0 == 0x0004) zero // OAMDATA
    else if (address0 == 0x0005) zero // PPUSCROLL
    else if (address0 == 0x0006) zero // PPUADDR
    else                              // PPUDATA
      for {
        d1          <- getData.toNesState
        vramAddress <- getLoopyV.toNesState
        d2          <- ppuRead(vramAddress.asUInt16)
        _           <- setData(d2).toNesState
        ctrl        <- getCtrl.toNesState
        v           <- getLoopyV.toNesState
        _           <- setLoopyV(LoopyAddress(v.asUInt16 + ctrl.incrementMode.delta)).toNesState
      } yield if (vramAddress.asUInt16 >= 0x3F00) d2 else d1
  }

  def cpuWrite(address: UInt16, d: UInt8): State[NesState, Unit] = {
    require(address >= 0x2000 && address <= 0x3FFF)
    require((d & 0xFF) == d)
    val default = State.pure[NesState, Unit]()
    val address0 = address & 0x7
    if (address0 == 0x0000)
      (
        for {
          _    <- setCtrl(d)
          ctrl <- getCtrl
          t1   <- getLoopyT
          t2   =  t1.setNametable(ctrl.nametable.id)
          _    <- setLoopyT(t2)
        } yield ()
      ).toNesState
    else if (address0 == 0x0001)
      State.modify(maskRegister.set(PpuMask(d))).toNesState
    else if (address0 == 0x0002)
      default
    else if (address0 == 0x0003)
      default
    else if (address0 == 0x0004)
      default
    else if (address0 == 0x0005)
      Monad[State[PpuState, *]].ifM(getLoopyW)(
        ifTrue = for {
          _  <- setLoopyX(d & 0x07)
          t1 <- getLoopyT
          t2 =  t1.setCoarseX((d >> 3) & 0x1F)
          _  <- setLoopyT(t2)
          _  <- setLoopyW(false)
        } yield (),
        ifFalse = for {
          t1 <- getLoopyT
          t2 =  t1.setCoarseY((d >> 3) & 0x1F).setFineY(d & 0x07)
          _  <- setLoopyT(t2)
          _  <- setLoopyW(true)
        } yield ()
      ).toNesState
    else if (address0 == 0x0006)
      Monad[State[PpuState, *]].ifM(getLoopyW)(
        ifTrue = for {
          t1 <- getLoopyT
          t2 =  LoopyAddress((t1.asUInt16 & 0xFF00) | d)
          _  <- setLoopyT(t2)
          _  <- setLoopyV(t2)
          _  <- setLoopyW(false)
        } yield (),
        ifFalse = for {
          t1 <- getLoopyT
          t2 =  LoopyAddress(((d & 0x3F) << 8) | (t1.asUInt16 & 0x00FF))
          _  <- setLoopyT(t2)
          _  <- setLoopyW(true)
        } yield ()
      ).toNesState
    else
      (
        for {
          ctrl  <- getCtrl
          vram1 <- getLoopyV
          vram2 =  LoopyAddress(vram1.asUInt16 + ctrl.incrementMode.delta)
          _     <- setLoopyV(vram2)
        } yield ()
      ).toNesState
  }

  def ppuRead(address: UInt16): State[NesState, UInt8] = {
    require((address & 0xFFFF) == address)
    val addr = address & 0x3FFF

    if (addr >= 0x0000 && addr <= 0x1FFF)
      Cartridge.ppuRead(addr)
    else if (addr >= 0x2000 && addr <= 0x3EFF)
      readNametables(addr).toNesState
    else if (addr >= 0x3F00 && addr <= 0x3FFF)
      (
        for {
          color <- getColor(addr)
          mask <- getMask
        } yield color & (if (mask.greyscale) 0x30 else 0xFF)
      ).toNesState
    else
      throw new RuntimeException("Invalid address!")
  }

  def ppuWrite(address: UInt16, d: UInt8): State[NesState, Unit] = {
    require((address & 0xFFFF) == address)
    val addr = address & 0x3FFF

    if (addr >= 0x0000 && addr <= 0x1FFF)
      Cartridge.ppuWrite(addr, d)
    else if (addr >= 0x2000 && addr <= 0x3EFF)
      writeNametables(addr, d).toNesState
    else if (addr >= 0x3F00 && addr <= 0x3FFF)
      setColor(address, d).toNesState
    else
      throw new RuntimeException("Invalid address!")
  }

  def clock: State[PpuState, Unit] = State.get.flatMap { s =>
    def isFetch(scanline: Int, cycle: Int): Boolean =
      (scanline >= -1 && scanline < 240) && ((cycle >= 2 && cycle < 258) || (cycle >= 321 && cycle < 338))

    (s.scanline, s.cycle) match {
      case (0, 0) =>
        incCycle
      case (-1, 1) =>
        setVerticalBlank(false)
      case (scanline, cycle) if isFetch(scanline, cycle) && ((cycle - 1) % 8 == 0) =>
        for {
          _ <- updateShifters()
          _ <- loadBgShifters
          v <- getLoopyV
          bgNextTileId <- readNametables(0x2000 | (v.asUInt16 & 0x0FFF))
          bgRenderingState <- getBgRenderingState
          updated = bgRenderingState.copy(nextTileId = bgNextTileId)
          _ <- setBgRenderingState(updated)
        } yield ()
      case (scanline, cycle) if isFetch(scanline, cycle) && ((cycle - 1) % 8 == 2) =>
        for {
          _ <- updateShifters()
          v <- getLoopyV
          address = 0x23C0 | (v.nametables << 10) | ((v.coarseY >> 2) << 3) | (v.coarseX >> 2)
          attr1 <- readNametables(address)
          attr2 = if (v.coarseY & 0x02) attr1 >> 4 else attr1
          attr3 = if (v.coarseX & 0x02) attr2 >> 2 else attr2
          attr4 = attr3 & 0x03
          bgRenderingState <- getBgRenderingState
          updated = bgRenderingState.copy(nextTileAttribute = attr4)
          _ <- setBgRenderingState(updated)
        } yield ()
      case (scanline, cycle) if isFetch(scanline, cycle) && ((cycle - 1) % 8 == 4) =>
        for {
          bgRenderingState <- getBgRenderingState
          ctrl <- getCtrl
          v <- getLoopyV
          address = ctrl.backgroundTableAddress.address + (bgRenderingState.nextTileId << 4) + v.fineY + 0
          updated = bgRenderingState.copy(nextTileLsb = address)
          _ <- setBgRenderingState(updated)
        } yield ()
      case (scanline, cycle) if isFetch(scanline, cycle) && ((cycle - 1) % 8 == 6) =>
        for {
          bgRenderingState <- getBgRenderingState
          ctrl <- getCtrl
          v <- getLoopyV
          address = ctrl.backgroundTableAddress.address + (bgRenderingState.nextTileId << 4) + v.fineY + 8
          updated = bgRenderingState.copy(nextTileLsb = address)
          _ <- setBgRenderingState(updated)
        } yield ()
      case (scanline, cycle) if isFetch(scanline, cycle) && ((cycle - 1) % 8 == 7) =>
        incScrollX
    }
  }

  def reset: State[PpuState, Unit] = ???

}
