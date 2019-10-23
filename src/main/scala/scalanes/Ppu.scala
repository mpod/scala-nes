package scalanes

import cats.Monad
import cats.data.State
import monocle.Lens
import monocle.macros.GenLens
import scalanes.AddressIncrementMode.AddressIncrementMode
import scalanes.BackgroundTableAddress.BackgroundTableAddress
import scalanes.MasterSlaveMode.MasterSlaveMode
import scalanes.NametableAddress.NametableAddress
import scalanes.NmiMode.NmiMode
import scalanes.SpriteSize.SpriteSize
import scalanes.SpriteTableAddress.SpriteTableAddress

import scala.language.implicitConversions

case class PpuState(patterns: Vector[Vector[UInt8]],
                    nametables: Vector[Vector[UInt8]],
                    palettes: Vector[Vector[UInt8]],
                    pixels: Vector[Int],
                    registers: PpuRegisters)

object PpuState {
  val registers: Lens[PpuState, PpuRegisters] = GenLens[PpuState](_.registers)
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

// http://wiki.nesdev.com/w/index.php/PPU_registers#PPUCTRL
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

// http://wiki.nesdev.com/w/index.php/PPU_registers#PPUMASK
case class PpuMask(grayscale: Boolean,
                   renderBackgroundLeft: Boolean,
                   renderSpritesLeft: Boolean,
                   renderBackground: Boolean,
                   renderSprites: Boolean,
                   enhanceRed: Boolean,
                   enhanceGreen: Boolean,
                   enhanceBlue: Boolean) {
  def asUInt8: UInt8 =
    (if (grayscale)            0x01 << 0 else 0x00) |
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

// http://wiki.nesdev.com/w/index.php/PPU_registers#PPUSTATUS
case class PpuStatus(spriteOverflow: Boolean, spriteZeroHit: Boolean, verticalBlank: Boolean) {
  def asUInt8: UInt8 = ???
}

object PpuStatus {
  val spriteOverflow: Lens[PpuStatus, Boolean] = GenLens[PpuStatus](_.spriteOverflow)
  val spriteZeroHit: Lens[PpuStatus, Boolean]  = GenLens[PpuStatus](_.spriteZeroHit)
  val verticalBlank: Lens[PpuStatus, Boolean]  = GenLens[PpuStatus](_.verticalBlank)

  def apply(a: UInt8): PpuStatus = new PpuStatus(a & 0x20, a & 0x40, a & 0x80)
}

// http://wiki.nesdev.com/w/index.php/PPU_scrolling#PPU_internal_registers
case class LoopyRegisters(v: LoopyAddress, t: LoopyAddress, x: UInt3, w: Boolean) {
  require((x & 0x7) == x)
}

object LoopyRegisters {
  val v: Lens[LoopyRegisters, LoopyAddress] = GenLens[LoopyRegisters](_.v)
  val t: Lens[LoopyRegisters, LoopyAddress] = GenLens[LoopyRegisters](_.t)
  val x: Lens[LoopyRegisters, UInt3] = GenLens[LoopyRegisters](_.x)
  val w: Lens[LoopyRegisters, Boolean] = GenLens[LoopyRegisters](_.w)
}

case class LoopyAddress(coarseX: UInt5, coarseY: UInt5, nametable: UInt2, fineY: UInt3) {
  require((coarseX & 0x1F) == coarseX)
  require((coarseY & 0x1F) == coarseY)
  require((nametable & 0x3) == nametable)
  require((fineY & 0x7) == fineY)

  def setCoarseX(d: UInt5): LoopyAddress = LoopyAddress.coarseX.set(d)(this)
  def setCoarseY(d: UInt5): LoopyAddress = LoopyAddress.coarseY.set(d)(this)
  def setNametable(d: UInt2): LoopyAddress = LoopyAddress.nametable.set(d)(this)
  def setFineY(d: UInt3): LoopyAddress = LoopyAddress.fineY.set(d)(this)

  def asUInt16: UInt16 = ((fineY & 0x7) << 12) | ((nametable & 0x3) << 10) | ((coarseY & 0x1F) << 5) | (coarseX & 0x1F)
}

object LoopyAddress {
  val coarseX: Lens[LoopyAddress, UInt5] = GenLens[LoopyAddress](_.coarseX)
  val coarseY: Lens[LoopyAddress, UInt5] = GenLens[LoopyAddress](_.coarseY)
  val nametable: Lens[LoopyAddress, UInt2] = GenLens[LoopyAddress](_.nametable)
  val fineY: Lens[LoopyAddress, UInt3] = GenLens[LoopyAddress](_.fineY)

  def apply(address: UInt16): LoopyAddress = new LoopyAddress(
    address & 0x1F,
    (address >> 5) & 0x1F,
    (address >> 10) & 0x3,
    (address >> 12) & 0x7
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

  def setVerticalBlank(d: Boolean): State[PpuState, Unit] =
    State.modify((statusRegister composeLens PpuStatus.verticalBlank).set(d))

  def getData: State[PpuState, UInt8] = State.inspect(PpuState.registers.get).map(_.data)

  def setData(d: UInt8): State[PpuState, Unit] =
    State.modify((PpuState.registers composeLens PpuRegisters.data).set(d))

  def getStatus: State[PpuState, PpuStatus] = State.inspect(statusRegister.get)

  def getCtrl: State[PpuState, PpuCtrl] = State.inspect(ctrlRegister.get)

  def setCtrl(d: UInt8): State[PpuState, Unit] =
    State.modify(ctrlRegister.set(PpuCtrl(d)))

  def setMask(d: UInt8): State[PpuState, Unit] =
    State.modify(maskRegister.set(PpuMask(d)))

  def clearLoopyW: State[PpuState, Unit] =
    State.modify((loopyRegisters composeLens LoopyRegisters.w).set(false))

  def getLoopyV: State[PpuState, LoopyAddress] =
    State.inspect((loopyRegisters composeLens LoopyRegisters.v).get)

  def incLoopyV(delta: UInt15): State[PpuState, LoopyAddress] = State { s =>
    val loopyV = loopyRegisters composeLens LoopyRegisters.v
    val updated = loopyV.modify(a => LoopyAddress(a.asUInt16 + delta))(s)
    (updated, loopyV.get(updated))
  }

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

  def cpuRead(address: UInt16): State[PpuState, UInt8] = {
    // The PPU exposes eight memory-mapped registers to the CPU, which are mapped to the
    // address range $2000-$2007. Addresses between $2008 and $3FFF are mirrored.
    require(address >= 0x2000 && address <= 0x3FFF)
    val address0 = address & 0x7
    if (address0 == 0x0000)      State.pure(0x00) // PPUCTRL
    else if (address0 == 0x0001) State.pure(0x00) // PPUMASK
    else if (address0 == 0x0002)                  // PPUSTATUS
      for {
        data <- getData
        status <- getStatus.map(_.asUInt8)
        _ <- clearLoopyW
        _ <- setVerticalBlank(false)
      } yield (status & 0xE0) | (data & 0x1F)
    else if (address0 == 0x0003) State.pure(0x00) // OAMADDR
    else if (address0 == 0x0004) State.pure(0x00) // OAMDATA
    else if (address0 == 0x0005) State.pure(0x00) // PPUSCROLL
    else if (address0 == 0x0006) State.pure(0x00) // PPUADDR
    else if (address0 == 0x0007)                  // PPUDATA
      for {
        d1 <- getData
        vramAddress <- getLoopyV
        d2 <- ppuRead(vramAddress.asUInt16)
        _ <- setData(d2)
        ctrl <- getCtrl
        _ <- incLoopyV(ctrl.incrementMode.delta)
      } yield if (vramAddress.asUInt16 >= 0x3F00) d2 else d1
    else State.pure(0x00)
  }

  def cpuWrite(address: UInt16, d: UInt8): State[PpuState, Unit] = {
    require(address >= 0x2000 && address <= 0x3FFF)
    require((d & 0xFF) == d)
    val address0 = address & 0x7
    if (address0 == 0x0000)
      for {
        _ <- setCtrl(d)
        ctrl <- getCtrl
        t1 <- getLoopyT
        t2 = t1.setNametable(ctrl.nametable.id)
        _ <- setLoopyT(t2)
      } yield Unit
    else if (address0 == 0x0001)
      State.modify(maskRegister.set(PpuMask(d)))
    else if (address0 == 0x0002)
      State.pure(Unit)
    else if (address0 == 0x0003)
      State.pure(Unit)
    else if (address0 == 0x0004)
      State.pure(Unit)
    else if (address0 == 0x0005)
      Monad[State[PpuState, *]].ifM(getLoopyW)(
        ifTrue = for {
          _ <- setLoopyX(d & 0x07)
          t1 <- getLoopyT
          t2 = t1.setCoarseX((d >> 3) & 0x1F)
          _ <- setLoopyT(t2)
          _ <- setLoopyW(false)
        } yield Unit,
        ifFalse = for {
          t1 <- getLoopyT
          t2 = t1.setCoarseY((d >> 3) & 0x1F).setFineY(d & 0x07)
          _ <- setLoopyT(t2)
          _ <- setLoopyW(true)
        } yield Unit
      )
    else if (address0 == 0x0006)
      Monad[State[PpuState, *]].ifM(getLoopyW)(
        ifTrue = for {
          t1 <- getLoopyT
          t2 = LoopyAddress((t1.asUInt16 & 0xFF00) | d)
          _ <- setLoopyT(t2)
          _ <- setLoopyV(t2)
          _ <- setLoopyW(false)
        } yield Unit,
        ifFalse = for {
          t1 <- getLoopyT
          t2 = LoopyAddress(((d & 0x3F) << 8) | (t1.asUInt16 & 0x00FF))
          _ <- setLoopyT(t2)
          _ <- setLoopyW(true)
        } yield Unit
      )
    else if (address0 == 0x0007)
      for {
        ctrl <- getCtrl
        vram1 <- getLoopyV
        vram2 = LoopyAddress(vram1.asUInt16 + ctrl.incrementMode.delta)
        _ <- setLoopyV(vram2)
      } yield Unit
    else
      State.pure(Unit)
  }

  def ppuRead(address: UInt16): State[PpuState, UInt8] = ???

  def ppuWrite(address: UInt16, d: UInt8): State[PpuState, Unit] = ???

  def clock: State[PpuState, Unit] = ???

  def reset: State[PpuState, Unit] = ???

}
