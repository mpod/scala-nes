package scalanes

import monocle.Lens
import monocle.macros.GenLens

case class NesState(ram: Vector[UInt8], cpuRegisters: CpuState)

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

  val initial: NesState = {
    val zero: UInt8 = 0
    val ram = Vector.fill(64 * 1024)(zero)
    val stkp = 0xFD
    val pc = 0
    val status = (0x00 | CpuFlags.U.bit)
    val cpuState = CpuState(zero, zero, zero, stkp, pc, status, 0)
    NesState(ram, cpuState)
  }

  def fromString(program: String): NesState = {
    val s = initial
    val offset = 0x8000
    val updated = program
      .sliding(2, 2)
      .map(Integer.parseInt(_, 16))
      .zipWithIndex
      .foldLeft(s.ram) { case (acc, (d, i)) =>
        acc.updated(offset + i, d)
      }
      .updated(0xFFFC, 0x00)
      .updated(0xFFFD, 0x80)
    (pc.set(offset) andThen ram.set(updated))(s)
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

