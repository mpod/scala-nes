package scalanes.mutable

import monocle.Lens
import scalanes.mutable.PpuState
import scalanes.mutable.CpuFlags.CpuFlags
import scalanes.mutable.Mirroring.Mirroring

class CpuState(
  var a: UInt8,
  var x: UInt8,
  var y: UInt8,
  var stkp: UInt8,
  var pc: UInt16,
  var status: UInt8,
  var cycles: Int,
  var haltAt: UInt16
) {

  def getFlag(flag: CpuFlags): Boolean = status & flag.bit

  def isValid: Boolean =
    isValidUInt8(a) & isValidUInt8(x) & isValidUInt8(y) & isValidUInt8(y) & isValidUInt8(stkp) &
    isValidUInt16(pc) & isValidUInt8(status) & isValidUInt16(haltAt)
}

object CpuState {
  val a: Lens[CpuState, UInt8] = lens(_.a, _.a_=)
  val x: Lens[CpuState, UInt8] = lens(_.x, _.x_=)
  val y: Lens[CpuState, UInt8] = lens(_.x, _.y_=)
  val stkp: Lens[CpuState, UInt8] = lens(_.stkp, _.stkp_=)
  val pc: Lens[CpuState, UInt16] = lens(_.pc, _.pc_=)
  val status: Lens[CpuState, UInt8] = lens(_.status, _.status_=)
  val cycles: Lens[CpuState, Int] = lens(_.cycles, _.cycles_=)
  val haltAt: Lens[CpuState, UInt16] = lens(_.haltAt, _.haltAt_=)

  def initial: CpuState =
    new CpuState(
     0x00,
     0x00,
     0x00,
     0xFD,
     0x0000,
     0x00 | CpuFlags.U.bit | CpuFlags.I.bit,
     0,
     0xFFFF
    )

}

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

class NesState(
  // 2KB internal RAM
  var ram: Vector[UInt8],
  var cpuState: CpuState,
  var ppuState: PpuState
)

object NesState {
  val ram: Lens[NesState, Vector[UInt8]] = lens(_.ram, _.ram_=)
  val cpuState: Lens[NesState, CpuState] = lens(_.cpuState, _.cpuState_=)
  val ppuState: Lens[NesState, PpuState] = lens(_.ppuState, _.ppuState_=)

  def initial(mirroring: Mirroring): NesState = new NesState(
    Vector.fill(0x800)(0x00),
    CpuState.initial,
    PpuState.initial(mirroring)
  )
}
