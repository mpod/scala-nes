package scalanes

import monocle.Lens
import monocle.macros.GenLens

case class NesState(ram: Vector[UInt8], cpuRegisters: CpuState, ppuState: PpuState, cartridge: Cartridge) {
  def isFrameComplete: Boolean = ppuState.scanline == -1 && ppuState.cycle == 0
}

object NesState {
  val ram: Lens[NesState, Vector[UInt8]] = GenLens[NesState](_.ram)
  val cpuRegisters: Lens[NesState, CpuState] = GenLens[NesState](_.cpuRegisters)
  val a: Lens[NesState, UInt8] = cpuRegisters composeLens GenLens[CpuState](_.a)
  val x: Lens[NesState, UInt8] = cpuRegisters composeLens GenLens[CpuState](_.x)
  val y: Lens[NesState, UInt8] = cpuRegisters composeLens GenLens[CpuState](_.y)
  val stkp: Lens[NesState, UInt8] = cpuRegisters composeLens GenLens[CpuState](_.stkp)
  val pc: Lens[NesState, UInt16] = cpuRegisters composeLens GenLens[CpuState](_.pc)
  val status: Lens[NesState, UInt8] = cpuRegisters composeLens GenLens[CpuState](_.status)
  val cycles: Lens[NesState, Int] = cpuRegisters composeLens GenLens[CpuState](_.cycles)
  val cartridge: Lens[NesState, Cartridge] = GenLens[NesState](_.cartridge)
  val ppuState: Lens[NesState, PpuState] = GenLens[NesState](_.ppuState)

  val initial: NesState = {
    val ram = Vector.fill(2 * 1024)(0)
    val stkp = 0xFD
    val pc = 0
    val status = 0x00 | CpuFlags.U.bit
    val cpuState = CpuState(0, 0, 0, stkp, pc, status, 0)
    val ppuState = PpuState(Vector.empty, Vector.empty, PpuRegisters.initial, Mirroring.Horizontal,
      0, 0, BgRenderingState.initial, Vector.empty)
    NesState(ram, cpuState, ppuState, Cartridge.empty)
  }

  def fromString(program: String): NesState = {
    val s = initial
    val offset = 0x8000
    val updated = Cartridge.fromString(program, offset)
    (pc.set(offset) andThen cartridge.set(updated))(s)
  }
}

case class CpuState(a: UInt8, x: UInt8, y: UInt8, stkp: UInt8, pc: UInt16, status: UInt8, cycles: Int)

object CpuFlags extends Enumeration {
  protected case class Val(bit: Int) extends super.Val
  type CpuFlags = Val
  val C: CpuFlags = Val(1 << 0)
  val Z: CpuFlags = Val(1 << 1)
  val I: CpuFlags = Val(1 << 2)
  val D: CpuFlags = Val(1 << 3)
  val B: CpuFlags = Val(1 << 4)
  val U: CpuFlags = Val(1 << 5)
  val V: CpuFlags = Val(1 << 6)
  val N: CpuFlags = Val(1 << 7)
}

