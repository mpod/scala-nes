package scalanes

import com.typesafe.scalalogging.LazyLogging

import scala.annotation.tailrec
import scala.language.implicitConversions

class CpuState(
  var a: UInt8,
  var x: UInt8,
  var y: UInt8,
  var stkp: UInt8,
  var pc: UInt16,
  var status: UInt8,
  var cycles: Long
) {
  def flagC: Boolean = status & (0x01 << 0)
  def flagZ: Boolean = status & (0x01 << 1)
  def flagI: Boolean = status & (0x01 << 2)
  def flagD: Boolean = status & (0x01 << 3)
  def flagB: Boolean = status & (0x01 << 4)
  def flagU: Boolean = status & (0x01 << 5)
  def flagV: Boolean = status & (0x01 << 6)
  def flagN: Boolean = status & (0x01 << 7)
}

object CpuState {
  val a: Setter[CpuState, UInt8]          = (a, s) => s.a = a
  val x: Setter[CpuState, UInt8]          = (a, s) => s.x = a
  val y: Setter[CpuState, UInt8]          = (a, s) => s.y = a
  val stkp: Setter[CpuState, UInt8]       = (a, s) => s.stkp = a
  val pc: Setter[CpuState, UInt16]        = (a, s) => s.pc = a
  val status: Setter[CpuState, UInt8]     = (a, s) => s.status = a
  val cycles: Incrementer[CpuState, Long] = (d, s) => s.cycles += d

  def apply(): CpuState =
    new CpuState(
      a = 0x00,
      x = 0x00,
      y = 0x00,
      stkp = 0xfd,
      pc = 0x0000,
      status = 0x00 | (0x01 << 5) | (0x01 << 2),
      cycles = 0
    )
}

object Cpu extends LazyLogging {

  trait AddressMode

  trait RwAddressMode extends AddressMode {
    def addressUnit: NesState => (NesState, AddressUnit)
  }

  trait RelAddressMode extends AddressMode {
    def address: NesState => (NesState, Byte)
  }

  trait AbsRwAddressMode extends RwAddressMode {
    override def addressUnit: NesState => (NesState, AbsAddressUnit)
  }

  trait AddressUnit {
    def read(nes: NesState): (NesState, UInt8)
    def write(d: UInt8)(nes: NesState): NesState
  }

  case class AbsAddressUnit(address: UInt16) extends AddressUnit {
    override def read(nes: NesState): (NesState, UInt8) = cpuRead(address)(nes)
    override def write(d: UInt8)(nes: NesState): NesState = {
      require((d & 0xff) == d)
      cpuWrite(address, d)(nes)
    }
  }

  case object AccAddressUnit extends AddressUnit {
    override def read(nes: NesState): (NesState, UInt8)   = (nes, nes.cpuState.a)
    override def write(d: UInt8)(nes: NesState): NesState = setA(d)(nes)
  }

  private val absAddressUnitLookup: Array[AbsAddressUnit] = (0x0000 to 0xffff).map(AbsAddressUnit).toArray

  type Op = NesState => NesState

  private def rwOp(addressMode: RwAddressMode)(op: (NesState, AddressUnit) => NesState): Op =
    nes => {
      val (nes1, au) = addressMode.addressUnit(nes)
      op(nes1, au)
    }

  def incPc(nes: NesState): NesState = incPc(1)(nes)

  def incPc(delta: UInt5)(nes: NesState): NesState = setPc((nes.cpuState.pc + delta) & 0xffff)(nes)

  def decPc(nes: NesState): NesState = incPc(-1)(nes)

  def setPc(d: UInt16)(nes: NesState): NesState = {
    val cpu = CpuState.pc.set(d)(nes.cpuState)
    NesState.cpuState.set(cpu)(nes)
  }

  def setStatus(d: UInt8)(nes: NesState): NesState = {
    val cpu = CpuState.status.set(d)(nes.cpuState)
    NesState.cpuState.set(cpu)(nes)
  }

  def setA(d: UInt8)(nes: NesState): NesState = {
    val cpu = CpuState.a.set(d)(nes.cpuState)
    NesState.cpuState.set(cpu)(nes)
  }

  def setX(d: UInt8)(nes: NesState): NesState = {
    val cpu = CpuState.x.set(d)(nes.cpuState)
    NesState.cpuState.set(cpu)(nes)
  }

  def setY(d: UInt8)(nes: NesState): NesState = {
    val cpu = CpuState.y.set(d)(nes.cpuState)
    NesState.cpuState.set(cpu)(nes)
  }

  def setStkp(d: UInt8)(nes: NesState): NesState = {
    val cpu = CpuState.stkp.set(d)(nes.cpuState)
    NesState.cpuState.set(cpu)(nes)
  }

  def incStkp(nes: NesState): NesState = setStkp((nes.cpuState.stkp + 1) & 0xff)(nes)

  def decStkp(nes: NesState): NesState = setStkp((nes.cpuState.stkp - 1) & 0xff)(nes)

  private def statusFlagSetter(pos: Int, d: UInt1): NesState => NesState =
    nes => {
      val status = (nes.cpuState.status & ~(0x01 << pos)) | (d << pos)
      val cpu    = CpuState.status.set(status)(nes.cpuState)
      NesState.cpuState.set(cpu)(nes)
    }

  private val setFlagC: NesState => NesState   = statusFlagSetter(0, 1)
  private val clearFlagC: NesState => NesState = statusFlagSetter(0, 0)
  private val setFlagZ: NesState => NesState   = statusFlagSetter(1, 1)
  private val clearFlagZ: NesState => NesState = statusFlagSetter(1, 0)
  private val setFlagI: NesState => NesState   = statusFlagSetter(2, 1)
  private val clearFlagI: NesState => NesState = statusFlagSetter(2, 0)
  private val setFlagD: NesState => NesState   = statusFlagSetter(3, 1)
  private val clearFlagD: NesState => NesState = statusFlagSetter(3, 0)
  private val setFlagB: NesState => NesState   = statusFlagSetter(4, 1)
  private val clearFlagB: NesState => NesState = statusFlagSetter(4, 0)
  private val setFlagU: NesState => NesState   = statusFlagSetter(5, 1)
  private val clearFlagU: NesState => NesState = statusFlagSetter(5, 0)
  private val setFlagV: NesState => NesState   = statusFlagSetter(6, 1)
  private val clearFlagV: NesState => NesState = statusFlagSetter(6, 0)
  private val setFlagN: NesState => NesState   = statusFlagSetter(7, 1)
  private val clearFlagN: NesState => NesState = statusFlagSetter(7, 0)

  def setFlagC(f: Boolean): NesState => NesState =
    if (f) setFlagC else clearFlagC

  def setFlagZ(f: Boolean): NesState => NesState =
    if (f) setFlagZ else clearFlagZ

  def setFlagI(f: Boolean): NesState => NesState =
    if (f) setFlagI else clearFlagI

  def setFlagD(f: Boolean): NesState => NesState =
    if (f) setFlagD else clearFlagD

  def setFlagB(f: Boolean): NesState => NesState =
    if (f) setFlagB else clearFlagB

  def setFlagU(f: Boolean): NesState => NesState =
    if (f) setFlagU else clearFlagU

  def setFlagV(f: Boolean): NesState => NesState =
    if (f) setFlagV else clearFlagV

  def setFlagN(f: Boolean): NesState => NesState =
    if (f) setFlagN else clearFlagN

  type FlagSetter = NesState => NesState

  def setFlags(f1: FlagSetter)(nes: NesState): NesState =
    f1(nes)
  def setFlags(f1: FlagSetter, f2: FlagSetter)(nes: NesState): NesState =
    f1(f2(nes))
  def setFlags(f1: FlagSetter, f2: FlagSetter, f3: FlagSetter)(nes: NesState): NesState =
    f1(f2(f3(nes)))
  def setFlags(f1: FlagSetter, f2: FlagSetter, f3: FlagSetter, f4: FlagSetter)(nes: NesState): NesState =
    f1(f2(f3(f4(nes))))

  def setZnFlags(d: UInt8)(nes: NesState): NesState =
    setFlags(
      setFlagZ(d == 0x00),
      setFlagN(d & 0x80)
    )(nes)

  def incCycles(d: Int, nes: NesState): NesState = {
    val cpu = CpuState.cycles.inc(d)(nes.cpuState)
    NesState.cpuState.set(cpu)(nes)
  }

  val ramReadLookup: Array[State[NesState, UInt8]] =
    (0x0000 to 0x1fff).map(address => State.inspect[NesState, UInt8](_.ram(address % 0x800))).toArray

  def cpuRead(address: UInt16): State[NesState, UInt8] = {
    require((address & 0xffff) == address)
    if (address >= 0x0000 && address <= 0x1fff) // RAM
      ramReadLookup(address)
    else if (address >= 0x2000 && address <= 0x3fff) // PPU registers
      Ppu.cpuRead(address)
    else if (address == 0x4015) // APU register
      Apu.cpuRead(0x4015)
    else if (address == 0x4016) // Controller 1
      Controller.serialReadController1
    else if (address == 0x4017) // Controller 2
      Controller.serialReadController2
    else if (address >= 0x6000 && address <= 0xffff) // Cartridge
      Cartridge.cpuRead(address)
    else
      throw new RuntimeException(f"Invalid cpu memory read at address $address%#04x")
  }

  def cpuRead16(address: UInt16, nes: NesState): (NesState, UInt16) = {
    val (nes1, lo) = cpuRead(address)(nes)
    val (nes2, hi) = cpuRead(address + 1)(nes1)
    (nes2, asUInt16(hi, lo))
  }

  def cpuRead16bug(address: UInt16, nes: NesState): (NesState, UInt16) = {
    val (nes1, lo) = cpuRead(address)(nes)
    val (nes2, hi) = cpuRead((address & 0xff00) | ((address + 1) & 0x00ff))(nes1)
    (nes2, asUInt16(hi, lo))
  }

  def cpuWrite(address: UInt16, d: UInt8)(nes: NesState): NesState =
    if (address >= 0x0000 && address < 0x2000) //RAM
      NesState.ram.set(address, d)(nes)
    else if (address >= 0x2000 && address < 0x4000) // PPU registers
      Ppu.cpuWrite(0x2000 + (address & 0x7), d)(nes)
    else if (address >= 0x4000 && address < 0x4014) // APU registers
      Apu.cpuWrite(address, d)(nes)
    else if (address == 0x4014) { // OAM DMA
      val delta = if ((nes.cpuState.cycles + 513) % 2 == 1) 513 + 1 else 513
      val nes1  = incCycles(delta, nes)
      Ppu.cpuWrite(address, d)(nes1)
    } else if (address == 0x4015) // APU registers
      Apu.cpuWrite(address, d)(nes)
    else if (address == 0x4016 && (d & 0x01)) // Controller 1
      Controller.writeController2(Controller.writeController1(nes))
    else if (address == 0x4017 && (d & 0x01)) // APU registers
      Apu.cpuWrite(address, d)(nes)
    else if (address < 0x6000)
      nes
    else if (address >= 0x6000 && address <= 0xffff) // Cartridge
      Cartridge.cpuWrite(address, d)(nes)
    else
      throw new RuntimeException(f"Invalid cpu memory write at address $address%#04x")

  val pop: State[NesState, UInt8] =
    nes => {
      val nes1      = incStkp(nes)
      val address   = (0x0100 + nes1.cpuState.stkp) & 0xffff
      val (nes2, d) = cpuRead(address)(nes1)
      (nes2, d)
    }

  def push(d: UInt8)(nes: NesState): NesState = {
    val address = (0x0100 + nes.cpuState.stkp) & 0xffff
    val nes1    = cpuWrite(address, d)(nes)
    decStkp(nes1)
  }

  def push16(d: UInt16)(nes: NesState): NesState = {
    val hi   = (d >> 8) & 0xff
    val lo   = d & 0xff
    val nes1 = push(hi)(nes)
    push(lo)(nes1)
  }

  private def isPageChange(a: Int, i: Int): Boolean = ((a + i) & 0xff00) != (a & 0xff00)

  private def asUInt16(hi: UInt8, lo: UInt8): UInt16 = {
    require((hi & 0xff) == hi)
    require((lo & 0xff) == lo)

    (hi << 8) | lo
  }

  def reset: NesState => NesState =
    nes => {
      val (nes1, lo) = cpuRead(0xfffc)(nes)
      val (nes2, hi) = cpuRead(0xfffd)(nes1)
      val pc         = asUInt16(hi, lo)
      val nes3       = NesState.cpuState.set(CpuState())(nes2)
      setPc(pc)(nes3)
    }

  def irq: Op =
    nes => {
      val pc1        = nes.cpuState.pc
      val pcHi       = (pc1 >> 8) & 0xff
      val pcLo       = pc1 & 0xff
      val nes1       = push(pcHi)(nes)
      val nes2       = push(pcLo)(nes1)
      val nes3       = setFlags(setFlagB(false), setFlagU(true), setFlagI(true))(nes2)
      val status     = nes1.cpuState.status
      val nes4       = push(status)(nes3)
      val (nes5, lo) = cpuRead(0xfffe)(nes4)
      val (nes6, hi) = cpuRead(0xfffe + 1)(nes5)
      val pc2        = asUInt16(hi, lo)
      val nes7       = setPc(pc2)(nes6)
      incCycles(7, nes7)
    }

  def nmi: State[NesState, NesState] =
    nes => {
      val pc1        = nes.cpuState.pc
      val pcHi       = (pc1 >> 8) & 0xff
      val pcLo       = pc1 & 0xff
      val nes1       = push(pcHi)(nes)
      val nes2       = push(pcLo)(nes1)
      val nes3       = setFlags(setFlagB(false), setFlagU(true), setFlagI(true))(nes2)
      val status     = nes1.cpuState.status
      val nes4       = push(status)(nes3)
      val (nes5, lo) = cpuRead(0xfffa)(nes4)
      val (nes6, hi) = cpuRead(0xfffa + 1)(nes5)
      val pc2        = asUInt16(hi, lo)
      val nes7       = setPc(pc2)(nes6)
      val nes8       = incCycles(7, nes7)
      (nes8, nes8)
    }

  def clock(nes: NesState): NesState = {
    val (nes1, address) = cpuRead(nes.cpuState.pc)(nes)
    val instr           = lookup(address)
    val nes2            = incCycles(instr.cycles, nes1)
    val nes3            = incPc(nes2)
    val nes4            = instr.op(nes3)
    setFlagU(true)(nes4)
  }

  // Implicit
  // It may operate on the accumulator.
  val IMP: RwAddressMode = new RwAddressMode {
    override def addressUnit: NesState => (NesState, AddressUnit) =
      (_, AccAddressUnit)
  }

  // Immediate - #v
  // Uses the 8-bit operand itself as the value for the operation, rather than fetching a value from a memory address.
  val IMM: AbsRwAddressMode = new AbsRwAddressMode {
    override def addressUnit: NesState => (NesState, AbsAddressUnit) =
      nes => {
        val address = nes.cpuState.pc
        (incPc(1)(nes), absAddressUnitLookup(address))
      }
  }

  // Zero page - d
  // Fetches the value from an 8-bit address on the zero page.
  val ZP0: AbsRwAddressMode = new AbsRwAddressMode {
    override def addressUnit: NesState => (NesState, AbsAddressUnit) =
      nes => {
        val (nes1, address) = cpuRead(nes.cpuState.pc)(nes)
        (incPc(1)(nes1), absAddressUnitLookup(address))
      }
  }

  // Zero page indexed - d,x
  val ZPX: AbsRwAddressMode = new AbsRwAddressMode {
    override def addressUnit: NesState => (NesState, AbsAddressUnit) =
      nes => {
        val (nes1, address) = cpuRead(nes.cpuState.pc)(nes)
        (incPc(1)(nes1), absAddressUnitLookup((address + nes1.cpuState.x) & 0xff))
      }
  }

  // Zero page indexed - d,y
  val ZPY: AbsRwAddressMode = new AbsRwAddressMode {
    override def addressUnit: NesState => (NesState, AbsAddressUnit) =
      nes => {
        val (nes1, address) = cpuRead(nes.cpuState.pc)(nes)
        (incPc(1)(nes1), absAddressUnitLookup((address + nes1.cpuState.y) & 0xff))
      }
  }

  // Relative
  val REL: RelAddressMode = new RelAddressMode {
    override def address: NesState => (NesState, Byte) =
      nes => {
        val (nes1, address) = cpuRead(nes.cpuState.pc)(nes)
        (incPc(1)(nes1), address.toByte)
      }
  }

  // Absolute - a
  // Fetches the value from a 16-bit address anywhere in memory.
  val ABS: AbsRwAddressMode = new AbsRwAddressMode {
    override def addressUnit: NesState => (NesState, AbsAddressUnit) =
      nes => {
        val (nes1, address) = cpuRead16(nes.cpuState.pc, nes)
        (incPc(2)(nes1), absAddressUnitLookup(address))
      }
  }

  // Absolute indexed - a,x
  val ABX: AbsRwAddressMode = new AbsRwAddressMode {
    override def addressUnit: NesState => (NesState, AbsAddressUnit) =
      nes => {
        val (nes1, address) = cpuRead16(nes.cpuState.pc, nes)
        (incPc(2)(nes1), absAddressUnitLookup((address + nes1.cpuState.x) & 0xffff))
      }
  }

  // Absolute indexed - a,y
  val ABY: AbsRwAddressMode = new AbsRwAddressMode {
    override def addressUnit: NesState => (NesState, AbsAddressUnit) =
      nes => {
        val (nes1, address) = cpuRead16(nes.cpuState.pc, nes)
        (incPc(2)(nes1), absAddressUnitLookup((address + nes1.cpuState.y) & 0xffff))
      }
  }

  // Indirect - (a)
  // The JMP instruction has a special indirect addressing mode that can jump to the address stored in a
  // 16-bit pointer anywhere in memory. It emulates a bug in the hardware that causes the low byte to wrap
  // without incrementing the high byte.
  val IND: AbsRwAddressMode = new AbsRwAddressMode {
    override def addressUnit: NesState => (NesState, AbsAddressUnit) =
      nes => {
        val (nes1, ptr)     = cpuRead16(nes.cpuState.pc, nes)
        val (nes2, address) = cpuRead16bug(ptr, nes1)
        (incPc(2)(nes2), absAddressUnitLookup(address))
      }
  }

  // Indexed indirect - (d,x)
  val IZX: AbsRwAddressMode = new AbsRwAddressMode {
    override def addressUnit: NesState => (NesState, AbsAddressUnit) =
      nes => {
        val (nes1, t)       = cpuRead(nes.cpuState.pc)(nes)
        val (nes2, address) = cpuRead16bug((t + nes1.cpuState.x) & 0x00ff, nes1)
        (incPc(1)(nes2), absAddressUnitLookup(address))
      }
  }

  // Indirect indexed - (d),y
  val IZY: AbsRwAddressMode = new AbsRwAddressMode {
    override def addressUnit: NesState => (NesState, AbsAddressUnit) =
      nes => {
        val (nes1, t) = cpuRead(nes.cpuState.pc)(nes)
        val (nes2, a) = cpuRead16bug(t, nes1)
        val address   = (a + nes1.cpuState.y) & 0xffff
        val c         = if (isPageChange(address, nes1.cpuState.y)) 1 else 0
        val nes3      = incPc(1)(nes2)
        val nes4      = incCycles(c, nes3)
        (nes4, absAddressUnitLookup(address))
      }
  }

  // Add with carry
  def ADC(addressMode: RwAddressMode): Op =
    rwOp(addressMode) { (nes, au) =>
      val (nes1, d) = au.read(nes)
      val cpu       = nes1.cpuState
      val c         = cpu.flagC
      val lsb       = if (c) 1 else 0
      val temp      = cpu.a + d + lsb
      val nes2 = setFlags(
        setFlagC(temp & 0xff00),
        setFlagZ((temp & 0x00ff) == 0x00),
        setFlagV((~(cpu.a ^ d) & (cpu.a ^ temp)) & 0x80),
        setFlagN(temp & 0x80)
      )(nes1)
      setA(temp & 0xff)(nes2)
    }

  // Logical AND
  def AND(addressMode: RwAddressMode): Op =
    rwOp(addressMode) { (nes, au) =>
      val (nes1, d) = au.read(nes)
      val r         = nes1.cpuState.a & d & 0xff
      val nes2      = setA(r)(nes1)
      setZnFlags(r)(nes2)
    }

  // Arithmetic shift left
  def ASL(addressMode: RwAddressMode): Op =
    rwOp(addressMode) { (nes, au) =>
      val (nes1, d) = au.read(nes)
      val temp      = d << 1
      val nes2 = setFlags(
        setFlagC(temp & 0xff00),
        setFlagZ((temp & 0x00ff) == 0x00),
        setFlagN(temp & 0x80)
      )(nes1)
      au.write(temp & 0xff)(nes2)
    }

  def branchIf(p: NesState => Boolean): Op =
    nes => {
      if (p(nes)) {
        val (nes1, relAddress) = REL.address(nes)
        val address            = (nes1.cpuState.pc + relAddress) & 0xffff
        val c                  = if (isPageChange(nes1.cpuState.pc, relAddress)) 2 else 1
        val nes2               = setPc(address)(nes1)
        incCycles(c, nes2)
      } else {
        incPc(1)(nes)
      }
    }

  // Branch if carry clear
  val BCC: Op = branchIf(!_.cpuState.flagC)

  // Branch if carry set
  val BCS: Op = branchIf(_.cpuState.flagC)

  // Branch if equal
  val BEQ: Op = branchIf(_.cpuState.flagZ)

  // Bit test
  def BIT(addressMode: RwAddressMode): Op =
    rwOp(addressMode) { (nes, au) =>
      val (nes1, d) = au.read(nes)
      val temp      = nes1.cpuState.a & d
      setFlags(
        setFlagZ((temp & 0x00ff) == 0x00),
        setFlagN(d & (1 << 7)),
        setFlagV(d & (1 << 6))
      )(nes1)
    }

  // Branch if minus
  val BMI: Op = branchIf(_.cpuState.flagN)

  // Branch if not equal
  val BNE: Op = branchIf(!_.cpuState.flagZ)

  // Branch if positive
  val BPL: Op = branchIf(!_.cpuState.flagN)

  // Force interrupt
  val BRK: Op =
    nes => {
      val nes1       = push16(nes.cpuState.pc)(nes)
      val nes2       = PHP(nes1)
      val nes3       = SEI(nes2)
      val (nes4, pc) = cpuRead16(0xfffe, nes3)
      setPc(pc)(nes4)
    }

  // Branch if overflow clear
  val BVC: Op = branchIf(!_.cpuState.flagV)

  // Branch if overflow set
  val BVS: Op = branchIf(_.cpuState.flagV)

  // Clear carry flag
  val CLC: Op = nes => setFlagC(false)(nes)

  // Clear decimal mode
  val CLD: Op = nes => setFlagD(false)(nes)

  // Clear interrupt disable
  val CLI: Op = nes => setFlagI(false)(nes)

  // Clear overflow flag
  val CLV: Op = nes => setFlagV(false)(nes)

  def compare(addressMode: RwAddressMode, getter: CpuState => UInt8): Op =
    rwOp(addressMode) { (nes, au) =>
      val (nes1, d1) = au.read(nes)
      val d2         = getter(nes1.cpuState)
      val temp       = d2 - d1
      setFlags(
        setFlagC(d2 >= d1),
        setFlagZ((temp & 0x00ff) == 0x0000),
        setFlagN(temp & 0x0080)
      )(nes1)
    }

  // Compare
  def CMP(addressMode: RwAddressMode): Op = compare(addressMode, _.a)

  // Compare X register
  def CPX(addressMode: RwAddressMode): Op = compare(addressMode, _.x)

  // Compare Y register
  def CPY(addressMode: RwAddressMode): Op = compare(addressMode, _.y)

  // Decrement memory
  def DEC(addressMode: RwAddressMode): Op =
    rwOp(addressMode) { (nes, au) =>
      val (nes1, d) = au.read(nes)
      val temp      = (d - 1) & 0xff
      val nes2      = setZnFlags(temp)(nes1)
      au.write(temp)(nes2)
    }

  // Decrement X register
  val DEX: Op =
    nes => {
      val temp = (nes.cpuState.x - 1) & 0xff
      val nes1 = setX(temp)(nes)
      setZnFlags(temp)(nes1)
    }

  // Decrement Y register
  val DEY: Op =
    nes => {
      val temp = (nes.cpuState.y - 1) & 0xff
      val nes1 = setY(temp)(nes)
      setZnFlags(temp)(nes1)
    }

  // Exclusive OR
  def EOR(addressMode: RwAddressMode): Op =
    rwOp(addressMode) { (nes, au) =>
      val (nes1, d) = au.read(nes)
      val temp      = (nes1.cpuState.a ^ d) & 0xff
      val nes2      = setA(temp)(nes1)
      setZnFlags(temp)(nes2)
    }

  // Increment memory
  def INC(addressMode: RwAddressMode): Op =
    rwOp(addressMode) { (nes, au) =>
      val (nes1, d) = au.read(nes)
      val temp      = (d + 1) & 0xff
      val nes2      = setZnFlags(temp)(nes1)
      au.write(temp)(nes2)
    }

  // Increment X register
  val INX: Op =
    nes => {
      val temp = (nes.cpuState.x + 1) & 0xff
      val nes1 = setX(temp)(nes)
      setZnFlags(temp)(nes1)
    }

  // Increment Y register
  val INY: Op =
    nes => {
      val temp = (nes.cpuState.y + 1) & 0xff
      val nes1 = setY(temp)(nes)
      setZnFlags(temp)(nes1)
    }

  // Jump
  def JMP(addressMode: AbsRwAddressMode): Op =
    nes => {
      val (nes1, au) = addressMode.addressUnit(nes)
      setPc(au.address)(nes1)
    }

  // Jump to subroutine
  def JSR(addressMode: AbsRwAddressMode): Op =
    nes => {
      val (nes1, au) = addressMode.addressUnit(nes)
      val nes2       = push16(nes1.cpuState.pc - 1)(nes1)
      setPc(au.address)(nes2)
    }

  // Load accumulator
  def LDA(addressMode: RwAddressMode): Op =
    rwOp(addressMode) { (nes, au) =>
      val (nes1, d) = au.read(nes)
      val nes2      = setA(d)(nes1)
      setZnFlags(d)(nes2)
    }

  // Load X register
  def LDX(addressMode: RwAddressMode): Op =
    rwOp(addressMode) { (nes, au) =>
      val (nes1, d) = au.read(nes)
      val nes2      = setX(d)(nes1)
      setZnFlags(d)(nes2)
    }

  // Load Y register
  def LDY(addressMode: RwAddressMode): Op =
    rwOp(addressMode) { (nes, au) =>
      val (nes1, d) = au.read(nes)
      val nes2      = setY(d)(nes1)
      setZnFlags(d)(nes2)
    }

  // Logical shift right
  def LSR(addressMode: RwAddressMode): Op =
    rwOp(addressMode) { (nes, au) =>
      val (nes1, d) = au.read(nes)
      val temp      = (d >> 1) & 0xff
      val nes2 = setFlags(
        setFlagC(d & 0x01),
        setFlagZ((temp & 0xff) == 0x00),
        setFlagN(temp & 0x80)
      )(nes1)
      au.write(temp)(nes2)
    }

  // No operation
  def NOP(addressMode: RwAddressMode): Op =
    rwOp(addressMode) { (nes, au) =>
      val (nes1, _) = au.read(nes)
      nes1
    }

  // Logical inclusive OR
  def ORA(addressMode: RwAddressMode): Op =
    rwOp(addressMode) { (nes, au) =>
      val (nes1, d) = au.read(nes)
      val temp      = (nes1.cpuState.a | d) & 0xff
      val nes2      = setA(temp)(nes1)
      setZnFlags(temp)(nes2)
    }

  // Push accumulator
  val PHA: Op =
    nes => push(nes.cpuState.a)(nes)

  // Push processor status
  val PHP: Op =
    nes => {
      val status = nes.cpuState.status | (0x01 << 4) | (0x01 << 5)
      val nes1   = push(status)(nes)
      setFlags(setFlagB(false), setFlagU(false))(nes1)
    }

  // Pull accumulator
  val PLA: Op =
    nes => {
      val (nes1, d) = pop(nes)
      val nes2      = setA(d)(nes1)
      setZnFlags(d)(nes2)
    }

  // Pull processor status
  val PLP: Op =
    nes => {
      val (nes1, d) = pop(nes)
      val nes2      = setStatus(d)(nes1)
      setFlagU(true)(nes2)
    }

  // Rotate left
  def ROL(addressMode: RwAddressMode): Op =
    rwOp(addressMode) { (nes, au) =>
      val (nes1, d) = au.read(nes)
      val lsb       = if (nes1.cpuState.flagC) 1 else 0
      val temp      = (d << 1) | lsb
      val nes2 = setFlags(
        setFlagC(temp & 0xff00),
        setFlagZ((temp & 0x00ff) == 0x00),
        setFlagN(temp & 0x80)
      )(nes1)
      au.write(temp & 0xff)(nes2)
    }

  // Rotate right
  def ROR(addressMode: RwAddressMode): Op =
    rwOp(addressMode) { (nes, au) =>
      val (nes1, d) = au.read(nes)
      val msb       = if (nes1.cpuState.flagC) 1 << 7 else 0
      val temp      = (d >> 1) | msb
      val nes2 = setFlags(
        setFlagC(d & 0x01),
        setFlagZ((temp & 0x00ff) == 0x00),
        setFlagN(temp & 0x80)
      )(nes1)
      au.write(temp & 0xff)(nes2)
    }

  // Return from interrupt
  val RTI: Op =
    nes => {
      val (nes1, d)   = pop(nes)
      val status      = d & ~(0x01 << 4) & ~(0x01 << 5)
      val nes2        = setStatus(status)(nes1)
      val (nes3, pc1) = pop(nes2)
      val (nes4, pc2) = pop(nes3)
      val pc          = asUInt16(pc2, pc1)
      setPc(pc)(nes4)
    }

  // Return from subroutine
  val RTS: Op =
    nes => {
      val (nes1, pc1) = pop(nes)
      val (nes2, pc2) = pop(nes1)
      val pc          = asUInt16(pc2, pc1)
      val nes3        = setPc(pc)(nes2)
      incPc(nes3)
    }

  // Subtract with carry
  def SBC(addressMode: RwAddressMode): Op =
    rwOp(addressMode) { (nes, au) =>
      val (nes1, d) = au.read(nes)
      val value     = d ^ 0x00ff
      val lsb       = if (nes1.cpuState.flagC) 1 else 0
      val temp      = nes1.cpuState.a + value + lsb
      val nes2 = setFlags(
        setFlagC(temp & 0xff00),
        setFlagZ((temp & 0x00ff) == 0x00),
        setFlagV((temp ^ nes1.cpuState.a) & (temp ^ value) & 0x80),
        setFlagN(temp & 0x80)
      )(nes1)
      setA(temp & 0xff)(nes2)
    }

  // Set carry flag
  val SEC: Op = nes => setFlagC(true)(nes)

  // Set decimal flag
  val SED: Op = nes => setFlagD(true)(nes)

  // Set enable interrupt
  val SEI: Op = nes => setFlagI(true)(nes)

  // Store accumulator
  def STA(addressMode: RwAddressMode): Op =
    rwOp(addressMode) { (nes, au) =>
      au.write(nes.cpuState.a)(nes)
    }

  // Store X register
  def STX(addressMode: RwAddressMode): Op =
    rwOp(addressMode) { (nes, au) =>
      au.write(nes.cpuState.x)(nes)
    }

  // Store Y register
  def STY(addressMode: RwAddressMode): Op =
    rwOp(addressMode) { (nes, au) =>
      au.write(nes.cpuState.y)(nes)
    }

  // Transfer accumulator to X
  val TAX: Op =
    nes => {
      val d    = nes.cpuState.a
      val nes1 = setX(d)(nes)
      setZnFlags(d)(nes1)
    }

  // Transfer accumulator to Y
  val TAY: Op =
    nes => {
      val d    = nes.cpuState.a
      val nes1 = setY(d)(nes)
      setZnFlags(d)(nes1)
    }

  // Transfer stack pointer to X
  val TSX: Op =
    nes => {
      val d    = nes.cpuState.stkp
      val nes1 = setX(d)(nes)
      setZnFlags(d)(nes1)
    }

  // Transfer X to accumulator
  val TXA: Op =
    nes => {
      val d    = nes.cpuState.x
      val nes1 = setA(d)(nes)
      setZnFlags(d)(nes1)
    }

  // Transfer X to stack pointer
  val TXS: Op =
    nes => setStkp(nes.cpuState.x)(nes)

  // Transfer Y to accumulator
  val TYA: Op =
    nes => {
      val d    = nes.cpuState.y
      val nes1 = setA(d)(nes)
      setZnFlags(d)(nes1)
    }

  // *** Unofficial instructions ***

  // Shortcut for LDA value then TAX
  def LAX(addressMode: RwAddressMode): Op =
    LDA(addressMode) andThen TAX

  // Stores the bitwise AND of A and X. As with STA and STX, no flags are affected.
  def SAX(addressMode: RwAddressMode): Op =
    rwOp(addressMode) { (nes, au) =>
      val d = nes.cpuState.a & nes.cpuState.x
      au.write(d)(nes)
    }

  // Equivalent to DEC value then CMP value
  def DCP(addressMode: RwAddressMode): Op =
    rwOp(addressMode) { (nes, au) =>
      val (nes1, d1) = au.read(nes)
      val nes2       = au.write((d1 - 1) & 0xff)(nes1)
      val (nes3, d2) = au.read(nes2)
      val d3         = nes3.cpuState.a
      val temp       = d3 - d2
      setFlags(
        setFlagC(d3 >= d2),
        setFlagZ((temp & 0x00ff) == 0x0000),
        setFlagN(temp & 0x0080)
      )(nes3)
    }

  // Equivalent to INC value then SBC value
  def ISC(addressMode: RwAddressMode): Op =
    rwOp(addressMode) { (nes, au) =>
      val (nes1, d1) = au.read(nes)
      val nes2       = au.write((d1 + 1) & 0xff)(nes1)
      val (nes3, d2) = au.read(nes2)
      val value      = d2 ^ 0x00ff
      val lsb        = if (nes3.cpuState.flagC) 1 else 0
      val temp       = nes3.cpuState.a + value + lsb
      val nes4       = setA(temp & 0xff)(nes3)
      setFlags(
        setFlagC(temp & 0xff00),
        setFlagZ((temp & 0x00ff) == 0x00),
        setFlagV((temp ^ nes3.cpuState.a) & (temp ^ value) & 0x80),
        setFlagN(temp & 0x80)
      )(nes4)
    }

  // Equivalent to ASL value then ORA value
  def SLO(addressMode: RwAddressMode): Op =
    rwOp(addressMode) { (nes, au) =>
      val (nes1, d1) = au.read(nes)
      val temp1      = d1 << 1
      val nes2       = setFlagC(temp1 & 0xff00)(nes1)
      val nes3       = au.write(temp1 & 0xff)(nes2)
      val temp2      = (nes3.cpuState.a | temp1) & 0xff
      val nes4       = setA(temp2)(nes3)
      setZnFlags(temp2)(nes4)
    }

  // Equivalent to ROL value then AND value
  def RLA(addressMode: RwAddressMode): Op =
    rwOp(addressMode) { (nes, au) =>
      val (nes1, d1) = au.read(nes)
      val lsb        = if (nes1.cpuState.flagC) 1 else 0
      val temp1      = (d1 << 1) | lsb
      val nes2       = setFlagC(temp1 & 0xff00)(nes1)
      val nes3       = au.write(temp1 & 0xff)(nes2)
      val temp2      = nes3.cpuState.a & temp1 & 0xff
      val nes4       = setA(temp2)(nes3)
      setZnFlags(temp2)(nes4)
    }

  // Equivalent to LSR value then EOR value
  def SRE(addressMode: RwAddressMode): Op =
    rwOp(addressMode) { (nes, au) =>
      val (nes1, d1) = au.read(nes)
      val temp1      = (d1 >> 1) & 0xff
      val nes2       = setFlagC(d1 & 0x01)(nes1)
      val nes3       = au.write(temp1 & 0xff)(nes2)
      val temp2      = (nes3.cpuState.a ^ temp1) & 0xff
      val nes4       = setA(temp2)(nes3)
      setZnFlags(temp2)(nes4)
    }

  // Equivalent to ROR value then ADC value
  def RRA(addressMode: RwAddressMode): Op =
    rwOp(addressMode) { (nes, au) =>
      val (nes1, d1) = au.read(nes)
      val msb        = if (nes1.cpuState.flagC) 1 << 7 else 0
      val temp1      = (d1 >> 1) | msb
      val nes2       = setFlagC(d1 & 0x01)(nes1)
      val nes3       = au.write(temp1 & 0xff)(nes2)
      val cpu        = nes3.cpuState
      val c          = cpu.flagC
      val lsb        = if (c) 1 else 0
      val temp2      = cpu.a + temp1 + lsb
      val nes4       = setA(temp2 & 0xff)(nes3)
      setFlags(
        setFlagC(temp2 & 0xff00),
        setFlagZ((temp2 & 0x00ff) == 0x00),
        setFlagV((~(cpu.a ^ temp1) & (cpu.a ^ temp2)) & 0x80),
        setFlagN(temp2 & 0x80)
      )(nes4)
    }

  def XXX: Op = identity

  case class Instr(info: String, op: Op, cycles: Int)

  // format: off
  val lookup: Map[UInt8, Instr] = Map(
    0x00 -> Instr("BRK/IMM", BRK,      7),     0x01 -> Instr("ORA/IMM", ORA(IZX), 6),
    0x05 -> Instr("ORA/ZP0", ORA(ZP0), 3),     0x06 -> Instr("ASL/ZP0", ASL(ZP0), 5),
    0x08 -> Instr("PHP/IMP", PHP,      3),     0x09 -> Instr("ORA/IMM", ORA(IMM), 2),
    0x0A -> Instr("ASL/IMP", ASL(IMP), 2),     0x0D -> Instr("ORA/ABS", ORA(ABS), 4),
    0x0E -> Instr("ASL/ABS", ASL(ABS), 6),     0x10 -> Instr("BPL/REL", BPL,      2),
    0x11 -> Instr("ORA/IZY", ORA(IZY), 5),     0x15 -> Instr("ORA/ZPX", ORA(ZPX), 4),
    0x16 -> Instr("ASL/ZPX", ASL(ZPX), 6),     0x18 -> Instr("CLC/IMP", CLC,      2),
    0x19 -> Instr("ORA/ABY", ORA(ABY), 4),     0x1D -> Instr("ORA/ABX", ORA(ABX), 4),
    0x1E -> Instr("ASL/ABX", ASL(ABX), 7),     0x20 -> Instr("JSR/ABS", JSR(ABS), 6),
    0x21 -> Instr("AND/IZX", AND(IZX), 6),     0x24 -> Instr("BIT/ZP0", BIT(ZP0), 3),
    0x25 -> Instr("AND/ZP0", AND(ZP0), 3),     0x26 -> Instr("ROL/IMM", ROL(ZP0), 5),
    0x28 -> Instr("PLP/IMP", PLP,      4),     0x29 -> Instr("AND/IMM", AND(IMM), 2),
    0x2A -> Instr("ROL/IMP", ROL(IMP), 2),     0x2C -> Instr("BIT/ABS", BIT(ABS), 4),
    0x2D -> Instr("AND/ABS", AND(ABS), 4),     0x2E -> Instr("ROL/ABS", ROL(ABS), 6),
    0x30 -> Instr("BMI/REL", BMI,      2),     0x31 -> Instr("AND/IZY", AND(IZY), 5),
    0x35 -> Instr("AND/ZPX", AND(ZPX), 4),     0x36 -> Instr("ROL/ZPX", ROL(ZPX), 6),
    0x38 -> Instr("SEC/IMP", SEC,      2),     0x39 -> Instr("AND/ABY", AND(ABY), 4),
    0x3D -> Instr("AND/ABX", AND(ABX), 4),     0x3E -> Instr("ROL/ABX", ROL(ABX), 7),
    0x40 -> Instr("RTI/IMP", RTI,      6),     0x41 -> Instr("EOR/IZX", EOR(IZX), 6),
    0x45 -> Instr("EOR/ZP0", EOR(ZP0), 3),     0x46 -> Instr("LSR/ZP0", LSR(ZP0), 5),
    0x48 -> Instr("PHA/IMP", PHA,      3),     0x49 -> Instr("EOR/IMM", EOR(IMM), 2),
    0x4A -> Instr("LSR/IMP", LSR(IMP), 2),     0x4C -> Instr("JMP/ABS", JMP(ABS), 3),
    0x4D -> Instr("EOR/ABS", EOR(ABS), 4),     0x4E -> Instr("LSR/ABS", LSR(ABS), 6),
    0x50 -> Instr("BVC/REL", BVC,      2),     0x51 -> Instr("EOR/IZY", EOR(IZY), 5),
    0x55 -> Instr("EOR/ZPX", EOR(ZPX), 4),     0x56 -> Instr("LSR/ZPX", LSR(ZPX), 6),
    0x58 -> Instr("CLI/IMP", CLI,      2),     0x59 -> Instr("EOR/ABY", EOR(ABY), 4),
    0x5D -> Instr("EOR/ABX", EOR(ABX), 4),     0x5E -> Instr("LSR/ABX", LSR(ABX), 7),
    0x60 -> Instr("RTS/IMP", RTS,      6),     0x61 -> Instr("ADC/IZX", ADC(IZX), 6),
    0x65 -> Instr("ADC/ZP0", ADC(ZP0), 3),     0x66 -> Instr("ROR/ZP0", ROR(ZP0), 5),
    0x68 -> Instr("PLA/IMP", PLA,      4),     0x69 -> Instr("ADC/IMM", ADC(IMM), 2),
    0x6A -> Instr("ROR/IMP", ROR(IMP), 2),     0x6C -> Instr("JMP/IND", JMP(IND), 5),
    0x6D -> Instr("ADC/ABS", ADC(ABS), 4),     0x6E -> Instr("ROR/ABS", ROR(ABS), 6),
    0x70 -> Instr("BVS/REL", BVS,      2),     0x71 -> Instr("ADC/IZY", ADC(IZY), 5),
    0x75 -> Instr("ADC/ZPX", ADC(ZPX), 4),     0x76 -> Instr("ROR/ZPX", ROR(ZPX), 6),
    0x78 -> Instr("SEI/IMP", SEI,      2),     0x79 -> Instr("ADC/ABY", ADC(ABY), 4),
    0x7D -> Instr("ADC/ABX", ADC(ABX), 4),     0x7E -> Instr("ROR/ABX", ROR(ABX), 7),
    0x81 -> Instr("STA/IZX", STA(IZX), 6),     0x84 -> Instr("STY/ZP0", STY(ZP0), 3),
    0x85 -> Instr("STA/ZP0", STA(ZP0), 3),     0x86 -> Instr("STX/ZP0", STX(ZP0), 3),
    0x88 -> Instr("DEY/IMP", DEY,      2),     0x8A -> Instr("TXA/IMP", TXA,      2),
    0x8C -> Instr("STY/ABS", STY(ABS), 4),     0x8D -> Instr("STA/ABS", STA(ABS), 4),
    0x8E -> Instr("STX/ABS", STX(ABS), 4),     0x90 -> Instr("BCC/REL", BCC,      2),
    0x91 -> Instr("STA/IZY", STA(IZY), 6),     0x94 -> Instr("STY/ZPX", STY(ZPX), 4),
    0x95 -> Instr("STA/ZPX", STA(ZPX), 4),     0x96 -> Instr("STX/ZPY", STX(ZPY), 4),
    0x98 -> Instr("TYA/IMP", TYA,      2),     0x99 -> Instr("STA/ABY", STA(ABY), 5),
    0x9A -> Instr("TXS/IMP", TXS,      2),     0x9D -> Instr("STA/ABX", STA(ABX), 5),
    0xA0 -> Instr("LDY/IMM", LDY(IMM), 2),     0xA1 -> Instr("LDA/IZX", LDA(IZX), 6),
    0xA2 -> Instr("LDX/IMM", LDX(IMM), 2),     0xA4 -> Instr("LDY/ZP0", LDY(ZP0), 3),
    0xA5 -> Instr("LDA/ZP0", LDA(ZP0), 3),     0xA6 -> Instr("LDX/ZP0", LDX(ZP0), 3),
    0xA8 -> Instr("TAY/IMP", TAY,      2),     0xA9 -> Instr("LDA/IMM", LDA(IMM), 2),
    0xAA -> Instr("TAX/IMP", TAX,      2),     0xAC -> Instr("LDY/ABS", LDY(ABS), 4),
    0xAD -> Instr("LDA/ABS", LDA(ABS), 4),     0xAE -> Instr("LDX/ABS", LDX(ABS), 4),
    0xB0 -> Instr("BCS/REL", BCS,      2),     0xB1 -> Instr("LDA/IZY", LDA(IZY), 5),
    0xB4 -> Instr("LDY/ZPX", LDY(ZPX), 4),     0xB5 -> Instr("LDA/ZPX", LDA(ZPX), 4),
    0xB6 -> Instr("LDX/ZPY", LDX(ZPY), 4),     0xB8 -> Instr("CLV/IMP", CLV,      2),
    0xB9 -> Instr("LDA/ABY", LDA(ABY), 4),     0xBA -> Instr("TSX/IMP", TSX,      2),
    0xBC -> Instr("LDY/ABX", LDY(ABX), 4),     0xBD -> Instr("LDA/ABX", LDA(ABX), 4),
    0xBE -> Instr("LDX/ABY", LDX(ABY), 4),     0xC0 -> Instr("CPY/IMM", CPY(IMM), 2),
    0xC1 -> Instr("CMP/IZX", CMP(IZX), 6),     0xC4 -> Instr("CPY/ZP0", CPY(ZP0), 3),
    0xC5 -> Instr("CMP/ZP0", CMP(ZP0), 3),     0xC6 -> Instr("DEC/ZP0", DEC(ZP0), 5),
    0xC8 -> Instr("INY/IMP", INY,      2),     0xC9 -> Instr("CMP/IMM", CMP(IMM), 2),
    0xCA -> Instr("DEX/IMP", DEX,      2),     0xCC -> Instr("CPY/ABS", CPY(ABS), 4),
    0xCD -> Instr("CMP/ABS", CMP(ABS), 4),     0xCE -> Instr("DEC/ABS", DEC(ABS), 6),
    0xD0 -> Instr("BNE/REL", BNE,      2),     0xD1 -> Instr("CMP/IZY", CMP(IZY), 5),
    0xD5 -> Instr("CMP/ZPX", CMP(ZPX), 4),     0xD6 -> Instr("DEC/ZPX", DEC(ZPX), 6),
    0xD8 -> Instr("CLD/IMP", CLD,      2),     0xD9 -> Instr("CMP/ABY", CMP(ABY), 4),
    0xDD -> Instr("CMP/ABX", CMP(ABX), 4),     0xDE -> Instr("DEC/ABX", DEC(ABX), 7),
    0xE0 -> Instr("CPX/IMM", CPX(IMM), 2),     0xE1 -> Instr("SBC/IZX", SBC(IZX), 6),
    0xE4 -> Instr("CPX/ZP0", CPX(ZP0), 3),     0xE5 -> Instr("SBC/ZP0", SBC(ZP0), 3),
    0xE6 -> Instr("INC/ZP0", INC(ZP0), 5),     0xE8 -> Instr("INX/IMP", INX,      2),
    0xE9 -> Instr("SBC/IMM", SBC(IMM), 2),     0xEA -> Instr("NOP/IMP", NOP(IMP), 2),
    0xEC -> Instr("CPX/ABS", CPX(ABS), 4),     0xED -> Instr("SBC/ABS", SBC(ABS), 4),
    0xEE -> Instr("INC/ABS", INC(ABS), 6),     0xF0 -> Instr("BEQ/REL", BEQ,      2),
    0xF1 -> Instr("SBC/IZY", SBC(IZY), 5),     0xF5 -> Instr("SBC/ZPX", SBC(ZPX), 4),
    0xF6 -> Instr("INC/ZPX", INC(ZPX), 6),     0xF8 -> Instr("SED/IMP", SED,      2),
    0xF9 -> Instr("SBC/ABY", SBC(ABY), 4),     0xFD -> Instr("SBC/ABX", SBC(ABX), 4),
    0xFE -> Instr("INC/ABX", INC(ABX), 7),

    // Unofficial opcodes
    0xA3 -> Instr("LAX/IZX", LAX(IZX), 6),     0xA7 -> Instr("LAX/ZP0", LAX(ZP0), 3),
    0xAB -> Instr("LAX/IMM", LAX(IMM), 2),     0xAF -> Instr("LAX/ABS", LAX(ABS), 4),
    0xB3 -> Instr("LAX/IZY", LAX(IZY), 5),     0xB7 -> Instr("LAX/ZPY", LAX(ZPY), 4),
    0xBF -> Instr("LAX/ABY", LAX(ABY), 4),     0x83 -> Instr("SAX/IZX", SAX(IZX), 6),
    0x87 -> Instr("SAX/ZP0", SAX(ZP0), 3),     0x8F -> Instr("SAX/ABS", SAX(ABS), 4),
    0x97 -> Instr("SAX/ZPY", SAX(ZPY), 4),     0xEB -> Instr("SBC/IMM", SBC(IMM), 2),
    0xC3 -> Instr("DCP/IZX", DCP(IZX), 8),     0xC7 -> Instr("DCP/ZP0", DCP(ZP0), 5),
    0xCF -> Instr("DCP/ABS", DCP(ABS), 6),     0xD3 -> Instr("DCP/IZY", DCP(IZY), 8),
    0xD7 -> Instr("DCP/ZPX", DCP(ZPX), 6),     0xDB -> Instr("DCP/ABY", DCP(ABY), 7),
    0xDF -> Instr("DCP/ABX", DCP(ABX), 7),     0xE3 -> Instr("ISC/IZX", ISC(IZX), 8),
    0xE7 -> Instr("ISC/ZP0", ISC(ZP0), 5),     0xEF -> Instr("ISC/ABS", ISC(ABS), 6),
    0xF3 -> Instr("ISC/IZY", ISC(IZY), 8),     0xF7 -> Instr("ISC/ZPX", ISC(ZPX), 6),
    0xFB -> Instr("ISC/ABY", ISC(ABY), 7),     0xFF -> Instr("ISC/ABX", ISC(ABX), 7),
    0x03 -> Instr("SLO/IZX", SLO(IZX), 8),     0x07 -> Instr("SLO/ZP0", SLO(ZP0), 5),
    0x0F -> Instr("SLO/ABS", SLO(ABS), 6),     0x13 -> Instr("SLO/IZY", SLO(IZY), 8),
    0x17 -> Instr("SLO/ZPX", SLO(ZPX), 6),     0x1B -> Instr("SLO/ABY", SLO(ABY), 7),
    0x1F -> Instr("SLO/ABX", SLO(ABX), 7),     0x23 -> Instr("RLA/IZX", RLA(IZX), 8),
    0x27 -> Instr("RLA/ZP0", RLA(ZP0), 5),     0x2F -> Instr("RLA/ABS", RLA(ABS), 6),
    0x33 -> Instr("RLA/IZY", RLA(IZY), 8),     0x37 -> Instr("RLA/ZPX", RLA(ZPX), 6),
    0x3B -> Instr("RLA/ABY", RLA(ABY), 7),     0x3F -> Instr("RLA/ABX", RLA(ABX), 7),
    0x43 -> Instr("SRE/IZX", SRE(IZX), 8),     0x47 -> Instr("SRE/ZP0", SRE(ZP0), 5),
    0x4F -> Instr("SRE/ABS", SRE(ABS), 6),     0x53 -> Instr("SRE/IZY", SRE(IZY), 8),
    0x57 -> Instr("SRE/ZPX", SRE(ZPX), 6),     0x5B -> Instr("SRE/ABY", SRE(ABY), 7),
    0x5F -> Instr("SRE/ABX", SRE(ABX), 7),     0x63 -> Instr("RRA/IZX", RRA(IZX), 8),
    0x67 -> Instr("RRA/ZP0", RRA(ZP0), 5),     0x6F -> Instr("RRA/ABS", RRA(ABS), 6),
    0x73 -> Instr("RRA/IZY", RRA(IZY), 8),     0x77 -> Instr("RRA/ZPX", RRA(ZPX), 6),
    0x7B -> Instr("RRA/ABY", RRA(ABY), 7),     0x7F -> Instr("RRA/ABX", RRA(ABX), 7)
    // format: on
  ).withDefault { d =>
    if (Set(0x80, 0x82, 0xc2, 0xe2, 0x89).contains(d))
      Instr("NOP/IMM", NOP(IMM), 2)
    else if (Set(0x1a, 0x3a, 0x5a, 0x7a, 0xda, 0xfa).contains(d))
      Instr("NOP/IMP", NOP(IMP), 2)
    else if (Set(0x0c).contains(d))
      Instr("NOP/ABS", NOP(ABS), 4)
    else if (Set(0x14, 0x34, 0x54, 0x74, 0xd4, 0xf4).contains(d))
      Instr("NOP/ZPX", NOP(ZPX), 4)
    else if (Set(0x1c, 0x3c, 0x5c, 0x7c, 0xdc, 0xfc).contains(d))
      Instr("NOP/ABX", NOP(ABX), 4)
    else if (Set(0x04, 0x44, 0x64).contains(d))
      Instr("NOP/ZP0", NOP(ZP0), 3)
    else if (
      Set(0x02, 0x12, 0x22, 0x32, 0x42, 0x52, 0x62, 0x72, 0x92, 0xb2, 0xd2, 0xf2, 0x0b, 0x2b, 0x4b, 0x6b, 0x8b, 0xcb)
        .contains(d)
    )
      Instr("XXX/IMP", XXX, 2)
    else if (Set(0xbb).contains(d))
      Instr("XXX/IMP", XXX, 4)
    else if (Set(0x9b, 0x9e, 0x9f).contains(d))
      Instr("XXX/IMP", XXX, 5)
    else if (Set(0x93).contains(d))
      Instr("XXX/IMP", XXX, 6)
    else {
      println(s"Invalid opcode: ${hex(d, 2)}")
      Instr("XXX/IMP", XXX, 8)
    }
  }

}
