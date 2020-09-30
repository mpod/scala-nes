package scalanes.mutable

import cats.Monad
import com.typesafe.scalalogging.LazyLogging
import scalanes.mutable.CpuFlags.CpuFlags
import scalanes.mutable.State.stateMonad

object Cpu extends LazyLogging {

  trait AddressMode

  trait RwAddressMode {
    def read: NesState => (NesState, UInt8)
    def write(d: UInt8): NesState => NesState
  }

  trait AbsAddressMode extends RwAddressMode {
    def prepareAddress(nes: NesState): (NesState, UInt16)

    override def read: NesState => (NesState, UInt8) =
      nes => {
        val (nes1, address) = prepareAddress(nes)
        cpuRead(address)(nes1)
      }

    override def write(d: UInt8): NesState => NesState =
      nes => {
        val (nes1, address) = prepareAddress(nes)
        cpuWrite(address, d)(nes1)
      }
  }

  trait RelAddressMode extends AddressMode {
    def prepareAddress(nes: NesState): (NesState, Byte)
  }

  type Op = NesState => NesState

  implicit class CpuStateOps[A](val a: State[CpuState, A]) extends AnyVal {
    def toNesState: State[NesState, A] = a.transformS(
      NesState.cpuState.get,
      (nesState, cpuState) => NesState.cpuState.set(cpuState)(nesState)
    )
  }

  private def lift(f: CpuState => CpuState): NesState => NesState =
    NesState.cpuState.modify(f)

  val incPc: NesState => NesState =
    lift(CpuState.pc.modify(pc => (pc + 1) & 0xffff))

  val decPc: NesState => NesState =
    lift(CpuState.pc.modify(pc => (pc - 1) & 0xffff))

  def setPc(d: UInt16): NesState => NesState =
    lift(CpuState.pc.set(d))

  def setCycles(d: UInt16): NesState => NesState =
    lift(CpuState.cycles.set(d))

  def incCycles(d: Int): NesState => NesState =
    lift(CpuState.cycles.modify(_ + d))

  def setStatus(d: UInt8): NesState => NesState =
    lift(CpuState.status.set(d))

  def setA(d: UInt8): NesState => NesState =
    lift(CpuState.a.set(d))

  def setX(d: UInt8): NesState => NesState =
    lift(CpuState.x.set(d))

  def setY(d: UInt8): NesState => NesState =
    lift(CpuState.y.set(d))

  def cpuRead(address: UInt16): State[NesState, UInt8] = {
    require((address & 0xffff) == address)

    if (address >= 0x0000 && address <= 0x1fff) // RAM
      State.inspect(_.ram(address % 0x800))
    else if (address >= 0x2000 && address <= 0x3fff) // PPU registers
      Ppu.cpuRead(address)
    else if (address == 0x4016) // Controller 1
      Controller.serialReadController1
    else if (address == 0x4017) // Controller 2
      Controller.serialReadController2
    else if (address >= 0x6000 && address <= 0xffff) // Cartridge
      Cartridge.cpuRead(address)
    else
      State.pure(0x00)
  }

  def cpuWrite(address: UInt16, d: UInt8): NesState => NesState =
    nes => {
      if (address >= 0x0000 && address <= 0x1fff) //RAM
        NesState.ram.modify(_.updated(address, d))(nes)
      else if (address >= 0x2000 && address <= 0x3fff) // PPU registers
        Ppu.cpuWrite(address, d).runS(nes)
      else if (address == 0x4014) { // OAM DMA
        ???
        /*
        val page = d << 8
        incCycles(513) *> (0 until 256)
          .map { oamAddress =>
            cpuRead(page | oamAddress).flatMap(Ppu.writeOam(oamAddress, _))
          }
          .reduce(_ *> _)
         */
      } else if (address == 0x4016 && (d & 0x01)) // Controller 1
        Controller.writeController1.runS(nes)
      else if (address == 0x4017 && (d & 0x01)) // Controller 2
        Controller.writeController2.runS(nes)
      else if (address >= 0x6000 && address <= 0xffff) // Cartridge
        Cartridge.cpuWrite(address, d).runS(nes)
      else
        nes
    }

  def modifyFlag(flag: CpuFlags, value: Boolean): NesState => NesState =
    modifyFlags(Map(flag -> value))

  def modifyFlags(flags: Map[CpuFlags, Boolean]): NesState => NesState =
    lift(
      CpuState.status.modify(s =>
        flags.foldLeft(s) { case (acc, (f, v)) =>
          if (v) acc | f.bit else acc & ~f.bit
        }
      )
    )

  def setZnFlags(d: UInt8): NesState => NesState =
    modifyFlags(
      Map(
        (CpuFlags.Z, d == 0x00),
        (CpuFlags.N, d & 0x80)
      )
    )

  def getFlag(flag: CpuFlags): State[NesState, Boolean] =
    State.inspect(flag.bit & _.cpuState.status)

  val pop: State[NesState, UInt8] =
    nes => {
      val nes1      = lift(CpuState.stkp.modify(stkp => (stkp + 1) & 0xff))(nes)
      val address   = (0x0100 + nes1.cpuState.stkp) & 0xffff
      val (nes2, d) = cpuRead(address)(nes1)
      (nes2, d)
    }

  def push(d: UInt8): NesState => NesState =
    nes => {
      val address   = (0x0100 + nes.cpuState.stkp) & 0xffff
      val modifyCpu = CpuState.stkp.modify(stkp => (stkp - 1) & 0xff)
      (cpuWrite(address, d) andThen lift(modifyCpu))(nes)
    }

  private def isPageChange(a: Int, i: Int): Boolean = ((a + i) & 0xff00) != (a & 0xff00)

  private def asUInt16(hi: UInt8, lo: UInt8): UInt16 = {
    require((hi & 0xff) == hi)
    require((lo & 0xff) == lo)

    (hi << 8) | lo
  }

  def reset: State[NesState, Unit] =
    nes => {
      val (nes1, lo) = cpuRead(0xfffc)(nes)
      val (nes2, hi) = cpuRead(0xfffd)(nes1)
      val pc         = asUInt16(hi, lo)
      val nes3 = lift(
        CpuState.pc.set(pc) andThen
          CpuState.a.set(0) andThen
          CpuState.x.set(0) andThen
          CpuState.y.set(0) andThen
          CpuState.stkp.set(0xfd) andThen
          CpuState.status.set(0x00 | CpuFlags.U.bit | CpuFlags.I.bit) andThen
          CpuState.cycles.set(0)
      )(nes2)
      (nes3, ())
    }

  def irq: Op =
    nes => {
      val pc1  = nes.cpuState.pc
      val pcHi = (pc1 >> 8) & 0xff
      val pcLo = pc1 & 0xff
      val flags = Map[CpuFlags, Boolean](
        (CpuFlags.B, false),
        (CpuFlags.U, true),
        (CpuFlags.I, true)
      )
      val nes1       = (push(pcHi) andThen push(pcLo) andThen modifyFlags(flags))(nes)
      val status     = nes1.cpuState.status
      val nes2       = push(status)(nes1)
      val (nes3, lo) = cpuRead(0xfffe)(nes2)
      val (nes4, hi) = cpuRead(0xfffe + 1)(nes3)
      val pc2        = asUInt16(hi, lo)
      (setPc(pc2) andThen setCycles(7))(nes4)
    }

  def nmi: State[NesState, NesState] =
    nes => {
      val pc1  = nes.cpuState.pc
      val pcHi = (pc1 >> 8) & 0xff
      val pcLo = pc1 & 0xff
      val flags = Map[CpuFlags, Boolean](
        (CpuFlags.B, false),
        (CpuFlags.U, true),
        (CpuFlags.I, true)
      )
      val nes1       = (push(pcHi) andThen push(pcLo) andThen modifyFlags(flags))(nes)
      val status     = nes1.cpuState.status
      val nes2       = push(status)(nes1)
      val (nes3, lo) = cpuRead(0xfffa)(nes2)
      val (nes4, hi) = cpuRead(0xfffa + 1)(nes3)
      val pc2        = asUInt16(hi, lo)
      val nes5       = (setPc(pc2) andThen setCycles(7))(nes4)
      (nes5, nes5)
    }

  val clock: State[NesState, NesState] =
    nes => {
      val nes2 = if (nes.cpuState.cycles == 0) {
        val (nes1, opCode) = cpuRead(nes.cpuState.pc)(nes)
        val instr          = lookup(opCode)
        (setCycles(instr.cycles) andThen incPc andThen instr.op andThen modifyFlag(CpuFlags.U, true))(nes1)
      } else {
        lift(CpuState.cycles.modify(_ - 1))(nes)
      }
      (nes2, nes2)
    }

  val executeNextInstr: State[NesState, Unit] =
    nes => {
      var nextNes = nes
      while (nextNes.cpuState.cycles > 0)
        nextNes = clock.runS(nextNes)
      (nextNes, ())
    }

  // Implicit
  // It may operate on the accumulator.
  val IMP: RwAddressMode = new RwAddressMode {
    override def read: NesState => (NesState, UInt8) =
      nes => (nes, nes.cpuState.a)

    override def write(d: UInt8): NesState => NesState =
      setA(d)
  }

  // Immediate - #v
  // Uses the 8-bit operand itself as the value for the operation, rather than fetching a value from a memory address.
  val IMM: AbsAddressMode =
    nes => {
      val address = nes.cpuState.pc
      val nes1    = incPc(nes)
      (nes1, address)
    }

  // Zero page - d
  // Fetches the value from an 8-bit address on the zero page.
  val ZP0: AbsAddressMode =
    nes => {
      val pc              = nes.cpuState.pc
      val (nes1, address) = cpuRead(pc)(nes)
      val nes2            = incPc(nes1)
      (nes2, address)
    }

  // Zero page indexed - d,x
  val ZPX: AbsAddressMode =
    nes => {
      val pc        = nes.cpuState.pc
      val (nes1, d) = cpuRead(pc)(nes)
      val address   = (d + nes1.cpuState.x) & 0xff
      val nes2      = incPc(nes1)
      (nes2, address)
    }

  // Zero page indexed - d,y
  val ZPY: AbsAddressMode =
    nes => {
      val pc        = nes.cpuState.pc
      val (nes1, d) = cpuRead(pc)(nes)
      val address   = (d + nes1.cpuState.y) & 0xff
      val nes2      = incPc(nes1)
      (nes2, address)
    }

  val REL: RelAddressMode =
    nes => {
      val pc              = nes.cpuState.pc
      val (nes1, address) = cpuRead(pc)(nes)
      val nes2            = incPc(nes1)
      (nes2, address.toByte)
    }

  // Absolute - a
  // Fetches the value from a 16-bit address anywhere in memory.
  val ABS: AbsAddressMode =
    nes => {
      val pc         = nes.cpuState.pc
      val (nes1, lo) = cpuRead(pc)(nes)
      val (nes2, hi) = cpuRead(pc + 1)(nes1)
      val address    = asUInt16(hi, lo)
      val nes3       = incPc(nes2)
      (nes3, address)
    }

  // Absolute indexed - a,x
  val ABX: AbsAddressMode =
    nes => {
      val (nes1, base) = ABS.prepareAddress(nes)
      val c            = if (isPageChange(base, nes1.cpuState.x)) 1 else 0
      val nes2         = incCycles(c)(nes1)
      val address      = (base + nes2.cpuState.x) & 0xffff
      (nes2, address)
    }

  // Absolute indexed - a,y
  val ABY: AbsAddressMode =
    nes => {
      val (nes1, base) = ABS.prepareAddress(nes)
      val c            = if (isPageChange(base, nes1.cpuState.y)) 1 else 0
      val nes2         = incCycles(c)(nes1)
      val address      = (base + nes2.cpuState.y) & 0xffff
      (nes2, address)
    }

  // Indirect - (a)
  // The JMP instruction has a special indirect addressing mode that can jump to the address stored in a
  // 16-bit pointer anywhere in memory.
  val IND: AbsAddressMode =
    nes => {
      val (nes1, ptr) = ABS.prepareAddress(nes)
      val (nes2, lo)  = cpuRead(ptr)(nes1)
      val (nes3, hi) =
        if ((ptr & 0x00ff) == 0x00ff)
          cpuRead(ptr & 0xff00)(nes2)
        else
          cpuRead((ptr + 1) & 0xffff)(nes2)
      val address = asUInt16(hi, lo)
      (nes3, address)
    }

  // Indexed indirect - (d,x)
  val IZX: AbsAddressMode =
    nes => {
      val pc         = nes.cpuState.pc
      val (nes1, t)  = cpuRead(pc)(nes)
      val x          = nes.cpuState.x
      val (nes2, lo) = cpuRead((t + x + 0) & 0x00ff)(nes1)
      val (nes3, hi) = cpuRead((t + x + 1) & 0x00ff)(nes2)
      val address    = asUInt16(hi, lo)
      val nes4       = incPc(nes3)
      (nes4, address)
    }

  // Indirect indexed - (d),y
  val IZY: AbsAddressMode =
    nes => {
      val pc         = nes.cpuState.pc
      val (nes1, t)  = cpuRead(pc)(nes)
      val y          = nes.cpuState.y
      val (nes2, lo) = cpuRead((t + 0) & 0x00ff)(nes1)
      val (nes3, hi) = cpuRead((t + 1) & 0x00ff)(nes2)
      val address    = asUInt16(hi, lo)
      val c          = if (isPageChange(address, y)) 1 else 0
      val nes4       = (incCycles(c) andThen incPc)(nes3)
      (nes4, (address + y) & 0xffff)
    }

  // Add with carry
  def ADC(addressMode: RwAddressMode): Op =
    nes => {
      val (nes1, d) = addressMode.read(nes)
      val cpu       = nes1.cpuState
      val c         = cpu.getFlag(CpuFlags.C)
      val lsb       = if (c) 1 else 0
      val temp      = cpu.a + d + lsb
      val flags = Map[CpuFlags, Boolean](
        CpuFlags.C -> (temp & 0xff00),
        CpuFlags.Z -> ((temp & 0x00ff) == 0x00),
        CpuFlags.V -> ((~(cpu.a ^ d) & (cpu.a ^ temp)) & 0x80),
        CpuFlags.N -> (temp & 0x80)
      )
      (setA(temp & 0xff) andThen modifyFlags(flags))(nes1)
    }

  // Logical AND
  def AND(addressMode: RwAddressMode): Op =
    nes => {
      val (nes1, d) = addressMode.read(nes)
      val cpu       = nes1.cpuState
      val r         = cpu.a & d & 0xff
      (setA(r) andThen setZnFlags(r))(nes1)
    }

  // Arithmetic shift left
  def ASL(addressMode: RwAddressMode): Op =
    nes => {
      val (nes1, d) = addressMode.read(nes)
      val temp      = d << 1
      val flags = Map[CpuFlags, Boolean](
        CpuFlags.C -> (temp & 0xff00),
        CpuFlags.Z -> ((temp & 0x00ff) == 0x00),
        CpuFlags.N -> (temp & 0x80)
      )
      (modifyFlags(flags) andThen addressMode.write(temp & 0xff))(nes1)
    }

  def branchIf(p: NesState => Boolean): Op =
    nes => {
      if (p(nes)) {
        val (nes1, relAddress) = REL.prepareAddress(nes)
        val cpu                = nes.cpuState
        val absAddress         = (cpu.pc + relAddress) & 0xffff
        val cycles             = cpu.cycles + (if (isPageChange(cpu.pc, relAddress)) 2 else 1)
        (setPc(absAddress) andThen setCycles(cycles))(nes1)
      } else {
        incPc(nes)
      }
    }

  // Branch if carry clear
  val BCC: Op = branchIf(!_.cpuState.getFlag(CpuFlags.C))

  // Branch if carry set
  val BCS: Op = branchIf(_.cpuState.getFlag(CpuFlags.C))

  // Branch if equal
  val BEQ: Op = branchIf(_.cpuState.getFlag(CpuFlags.Z))

  // Bit test
  def BIT(addressMode: RwAddressMode): Op =
    nes => {
      val (nes1, d) = addressMode.read(nes)
      val cpu       = nes1.cpuState
      val temp      = cpu.a & d
      val flags = Map[CpuFlags, Boolean](
        CpuFlags.Z -> ((temp & 0x00ff) == 0x00),
        CpuFlags.N -> (d & (1 << 7)),
        CpuFlags.V -> (d & (1 << 6))
      )
      modifyFlags(flags)(nes1)
    }

  // Branch if minus
  val BMI: Op = branchIf(_.cpuState.getFlag(CpuFlags.N))

  // Branch if not equal
  val BNE: Op = branchIf(!_.cpuState.getFlag(CpuFlags.Z))

  // Branch if positive
  val BPL: Op = branchIf(!_.cpuState.getFlag(CpuFlags.N))

  // Force interrupt
  def BRK: Op =
    nes => {
      val nes1 = incPc(nes)
      val nes2 = (
        modifyFlag(CpuFlags.I, value = true) andThen
          push((nes1.cpuState.pc >> 8) & 0xff) andThen push(nes1.cpuState.pc & 0xff)
          andThen modifyFlag(CpuFlags.B, value = true)
      )(nes1)
      val status     = nes2.cpuState.status
      val nes3       = (push(status) andThen modifyFlag(CpuFlags.B, value = false))(nes2)
      val (nes4, d1) = cpuRead(0xfffe)(nes3)
      val (nes5, d2) = cpuRead(0xffff)(nes4)
      val pc         = asUInt16(d2, d1)
      setPc(pc)(nes5)
    }

  // Branch if overflow clear
  val BVC: Op = branchIf(!_.cpuState.getFlag(CpuFlags.V))

  // Branch if overflow set
  val BVS: Op = branchIf(_.cpuState.getFlag(CpuFlags.V))

  // Clear carry flag
  val CLC: Op = modifyFlag(CpuFlags.C, value = false)

  // Clear decimal mode
  val CLD: Op = modifyFlag(CpuFlags.D, value = false)

  // Clear interrupt disable
  val CLI: Op = modifyFlag(CpuFlags.I, value = false)

  // Clear overflow flag
  val CLV: Op = modifyFlag(CpuFlags.V, value = false)

  def compare(addressMode: RwAddressMode, getter: CpuState => UInt8): NesState => NesState =
    nes => {
      val (nes1, d1) = addressMode.read(nes)
      val cpu        = nes1.cpuState
      val d2         = getter(cpu)
      val temp       = d2 - d1
      val flags = Map[CpuFlags, Boolean](
        CpuFlags.C -> (d2 >= d1),
        CpuFlags.Z -> ((temp & 0x00ff) == 0x0000),
        CpuFlags.N -> (temp & 0x0080)
      )
      modifyFlags(flags)(nes1)
    }

  // Compare
  def CMP(addressMode: RwAddressMode): Op = compare(addressMode, _.a)

  // Compare X register
  def CPX(addressMode: RwAddressMode): Op = compare(addressMode, _.x)

  // Compare Y register
  def CPY(addressMode: RwAddressMode): Op = compare(addressMode, _.y)

  // Decrement memory
  def DEC(addressMode: RwAddressMode): Op =
    nes => {
      val (nes1, d) = addressMode.read(nes)
      val temp      = (d - 1) & 0xff
      (setZnFlags(temp) andThen addressMode.write(temp))(nes1)
    }

  // Decrement X register
  val DEX: Op =
    nes => {
      val temp = (nes.cpuState.x - 1) & 0xff
      (setX(temp) andThen setZnFlags(temp))(nes)
    }

  // Decrement Y register
  val DEY: Op =
    nes => {
      val temp = (nes.cpuState.y - 1) & 0xff
      (setY(temp) andThen setZnFlags(temp))(nes)
    }

  // Exclusive OR
  def EOR(addressMode: RwAddressMode): Op =
    nes => {
      val (nes1, d) = addressMode.read(nes)
      val cpu       = nes1.cpuState
      val temp      = (cpu.a ^ d) & 0xff
      (setA(temp) andThen setZnFlags(temp))(nes1)
    }

  // Increment memory
  def INC(addressMode: RwAddressMode): Op =
    nes => {
      val (nes1, d) = addressMode.read(nes)
      val temp      = (d + 1) & 0xff
      (setZnFlags(temp) andThen addressMode.write(temp))(nes1)
    }

  // Increment X register
  val INX: Op =
    nes => {
      val temp = (nes.cpuState.x + 1) & 0xff
      (setX(temp) andThen setZnFlags(temp))(nes)
    }

  // Increment Y register
  val INY: Op =
    nes => {
      val temp = (nes.cpuState.y + 1) & 0xff
      (setY(temp) andThen setZnFlags(temp))(nes)
    }

  // Jump
  def JMP(addressMode: AbsAddressMode): Op =
    nes => {
      val (nes1, address) = addressMode.prepareAddress(nes)
      setPc(address)(nes1)
    }

  // Jump to subroutine
  def JSR(addressMode: AbsAddressMode): Op =
    nes => {
      val (nes1, address) = addressMode.prepareAddress(nes)
      val nes2            = decPc(nes1)
      val pcHi            = (nes2.cpuState.pc >> 8) & 0xff
      val pcLo            = nes2.cpuState.pc & 0xff
      val nes3            = (push(pcHi) andThen push(pcLo))(nes2)
      setPc(address)(nes3)
    }

  // Load accumulator
  def LDA(addressMode: RwAddressMode): Op =
    nes => {
      val (nes1, d) = addressMode.read(nes)
      (setA(d) andThen setZnFlags(d))(nes1)
    }

  // Load X register
  def LDX(addressMode: RwAddressMode): Op =
    nes => {
      val (nes1, d) = addressMode.read(nes)
      (setX(d) andThen setZnFlags(d))(nes1)
    }

  // Load Y register
  def LDY(addressMode: RwAddressMode): Op =
    nes => {
      val (nes1, d) = addressMode.read(nes)
      (setY(d) andThen setZnFlags(d))(nes1)
    }

  // Logical shift right
  def LSR(addressMode: RwAddressMode): Op =
    nes => {
      val (nes1, d) = addressMode.read(nes)
      val temp      = (d >> 1) & 0xff
      val flags = Map[CpuFlags, Boolean](
        CpuFlags.C -> (d & 0x01),
        CpuFlags.Z -> ((temp & 0xff) == 0x00),
        CpuFlags.N -> (temp & 0x80)
      )
      (modifyFlags(flags) andThen addressMode.write(temp))(nes1)
    }

  // No operation
  def NOP(addressMode: RwAddressMode): Op =
    nes => addressMode.read(nes)._1

  // Logical inclusive OR
  def ORA(addressMode: RwAddressMode): Op =
    nes => {
      val (nes1, d) = addressMode.read(nes)
      val temp      = (nes1.cpuState.a | d) & 0xff
      (setA(temp) andThen setZnFlags(temp))(nes1)
    }

  // Push accumulator
  val PHA: Op =
    nes => push(nes.cpuState.a)(nes)

  // Push processor status
  val PHP: Op =
    nes => {
      val status = nes.cpuState.status | CpuFlags.B.bit | CpuFlags.U.bit
      val flags = Map[CpuFlags, Boolean](
        CpuFlags.B -> false,
        CpuFlags.U -> false
      )
      (push(status) andThen modifyFlags(flags))(nes)
    }

  // Pull accumulator
  val PLA: Op =
    nes => {
      val (nes1, d) = pop(nes)
      (setA(d) andThen setZnFlags(d))(nes1)
    }

  // Pull processor status
  val PLP: Op =
    nes => {
      val (nes1, d) = pop(nes)
      (setStatus(d) andThen modifyFlag(CpuFlags.U, value = true))(nes1)
    }

  // Rotate left
  def ROL(addressMode: RwAddressMode): Op =
    nes => {
      val (nes1, d) = addressMode.read(nes)
      val c         = nes1.cpuState.getFlag(CpuFlags.C)
      val lsb       = if (c) 1 else 0
      val temp      = (d << 1) | lsb
      val flags = Map[CpuFlags, Boolean](
        CpuFlags.C -> (temp & 0xff00),
        CpuFlags.Z -> ((temp & 0x00ff) == 0x00),
        CpuFlags.N -> (temp & 0x80)
      )
      (modifyFlags(flags) andThen addressMode.write(temp & 0xff))(nes1)
    }

  // Rotate right
  def ROR(addressMode: RwAddressMode): Op =
    nes => {
      val (nes1, d) = addressMode.read(nes)
      val c         = nes1.cpuState.getFlag(CpuFlags.C)
      val msb       = if (c) 1 << 7 else 0
      val temp      = (d >> 1) | msb
      val flags = Map[CpuFlags, Boolean](
        CpuFlags.C -> (d & 0x01),
        CpuFlags.Z -> ((temp & 0x00ff) == 0x00),
        CpuFlags.N -> (temp & 0x80)
      )
      (modifyFlags(flags) andThen addressMode.write(temp & 0xff))(nes1)
    }

  // Return from interrupt
  val RTI: Op =
    nes => {
      val (nes1, d)   = pop(nes)
      val status      = d & ~CpuFlags.B.bit & ~CpuFlags.U.bit
      val (nes2, pc1) = (setStatus(status) andThen pop)(nes1)
      val (nes3, pc2) = pop(nes2)
      val pc          = asUInt16(pc2, pc1)
      setPc(pc)(nes3)
    }

  // Return from subroutine
  val RTS: Op =
    nes => {
      val (nes1, pc1) = pop(nes)
      val (nes2, pc2) = pop(nes1)
      val pc          = asUInt16(pc2, pc1)
      (setPc(pc) andThen incPc)(nes2)
    }

  // Subtract with carry
  def SBC(addressMode: RwAddressMode): Op =
    nes => {
      val (nes1, d) = addressMode.read(nes)
      val value     = d ^ 0x00ff
      val c         = nes1.cpuState.getFlag(CpuFlags.C)
      val lsb       = if (c) 1 else 0
      val temp      = nes1.cpuState.a + value + lsb
      val flags = Map[CpuFlags, Boolean](
        (CpuFlags.C, temp & 0xff00),
        (CpuFlags.Z, (temp & 0x00ff) == 0x00),
        (CpuFlags.V, (temp ^ nes1.cpuState.a) & (temp ^ value) & 0x80),
        (CpuFlags.N, temp & 0x80)
      )
      (setA(temp & 0xff) andThen modifyFlags(flags))(nes1)
    }

  // Set carry flag
  val SEC: Op = modifyFlag(CpuFlags.C, value = true)

  // Set decimal flag
  val SED: Op = modifyFlag(CpuFlags.D, value = true)

  // Set interrupt disable
  val SEI: Op = modifyFlag(CpuFlags.I, value = true)

  // Store accumulator
  def STA(addressMode: RwAddressMode): Op =
    nes => addressMode.write(nes.cpuState.a)(nes)

  // Store X register
  def STX(addressMode: RwAddressMode): Op =
    nes => addressMode.write(nes.cpuState.x)(nes)

  // Store Y register
  def STY(addressMode: RwAddressMode): Op =
    nes => addressMode.write(nes.cpuState.y)(nes)

  // Transfer accumulator to X
  val TAX: Op =
    nes => {
      val d = nes.cpuState.a
      (setX(d) andThen setZnFlags(d))(nes)
    }

  // Transfer accumulator to Y
  val TAY: Op =
    nes => {
      val d = nes.cpuState.a
      (setY(d) andThen setZnFlags(d))(nes)
    }

  // Transfer stack pointer to X
  val TSX: Op =
    nes => {
      val d = nes.cpuState.stkp
      (setX(d) andThen setZnFlags(d))(nes)
    }

  // Transfer X to accumulator
  val TXA: Op =
    nes => {
      val d = nes.cpuState.x
      (setA(d) andThen setZnFlags(d))(nes)
    }

  // Transfer X to stack pointer
  val TXS: Op =
    nes => lift(CpuState.stkp.set(nes.cpuState.x))(nes)

  // Transfer Y to accumulator
  val TYA: Op =
    nes => {
      val d = nes.cpuState.y
      (setA(d) andThen setZnFlags(d))(nes)
    }

  // *** Unofficial instructions ***

  /*
  // Shortcut for LDA value then TAX
  def LAX(addressMode: RwAddressMode): Op =
    for {
      _ <- LDA(addressMode)
      _ <- TAX
    } yield ()

  // Stores the bitwise AND of A and X. As with STA and STX, no flags are affected.
  def SAX(addressMode: RwAddressMode): Op =
    for {
      address <- addressMode
      a       <- getA
      x       <- getX
      d = a & x
      _ <- address.write(d)
    } yield ()

  // Equivalent to DEC value then CMP value
  def DCP(addressMode: RwAddressMode): Op =
    for {
      address <- addressMode
      d       <- address.read()
      temp = (d - 1) & 0xff
      _ <- address.write(temp)
      _ <- modifyFlag(CpuFlags.Z, temp == 0x00)
      _ <- modifyFlag(CpuFlags.N, temp & 0x80)
      _ <- compareS(address.read(), getA)
    } yield ()

  // Equivalent to INC value then SBC value
  def ISC(addressMode: RwAddressMode): Op =
    for {
      address <- addressMode
      d       <- address.read()
      temp1 = (d + 1) & 0xff
      _ <- address.write(temp1)
      value = temp1 ^ 0x00ff
      a <- getA
      c <- getFlag(CpuFlags.C)
      lsb   = if (c) 1 else 0
      temp2 = a + value + lsb
      _ <- modifyFlag(CpuFlags.C, temp2 & 0xff00)
      _ <- modifyFlag(CpuFlags.Z, (temp2 & 0x00ff) == 0x00)
      _ <- modifyFlag(CpuFlags.V, (temp2 ^ a) & (temp2 ^ value) & 0x80)
      _ <- modifyFlag(CpuFlags.N, temp2 & 0x80)
      _ <- setA(temp2 & 0xff)
    } yield ()

  // Equivalent to ASL value then ORA value
  def SLO(addressMode: RwAddressMode): Op =
    for {
      address <- addressMode
      d       <- address.read()
      temp1 = d << 1
      _ <- modifyFlag(CpuFlags.C, temp1 & 0xff00)
      _ <- address.write(temp1 & 0x00ff)
      a <- getA
      temp2 = (a | temp1) & 0xff
      _ <- setA(temp2)
      _ <- modifyFlag(CpuFlags.Z, temp2 == 0x00)
      _ <- modifyFlag(CpuFlags.N, temp2 & 0x80)
    } yield ()

  // Equivalent to ROL value then AND value
  def RLA(addressMode: RwAddressMode): Op =
    for {
      address <- addressMode
      d       <- address.read()
      c       <- getFlag(CpuFlags.C)
      lsb  = if (c) 1 else 0
      temp = (d << 1) | lsb
      _ <- modifyFlag(CpuFlags.C, temp & 0xff00)
      _ <- address.write(temp & 0xff)
      a <- getA
      r = a & temp & 0xff
      _ <- setA(r)
      _ <- modifyFlag(CpuFlags.Z, r == 0x00)
      _ <- modifyFlag(CpuFlags.N, r & 0x80)
    } yield ()

  // Equivalent to LSR value then EOR value
  def SRE(addressMode: RwAddressMode): Op =
    for {
      address <- addressMode
      d       <- address.read()
      _       <- modifyFlag(CpuFlags.C, d & 0x01)
      temp1 = (d >> 1) & 0xff
      _ <- address.write(temp1)
      a <- getA
      temp2 = (a ^ temp1) & 0xff
      _ <- setA(temp2)
      _ <- modifyFlag(CpuFlags.Z, temp2 == 0x00)
      _ <- modifyFlag(CpuFlags.N, temp2 & 0x80)
    } yield ()

  // Equivalent to ROR value then ADC value
  def RRA(addressMode: RwAddressMode): Op =
    for {
      address <- addressMode
      d       <- address.read()
      c       <- getFlag(CpuFlags.C)
      msb   = if (c) 1 << 7 else 0
      temp1 = (d >> 1) | msb
      _ <- modifyFlag(CpuFlags.C, d & 0x01)
      _ <- address.write(temp1 & 0xff)
      a <- getA
      c <- getFlag(CpuFlags.C)
      lsb   = if (c) 1 else 0
      temp2 = a + temp1 + lsb
      _ <- modifyFlag(CpuFlags.C, temp2 & 0xff00)
      _ <- modifyFlag(CpuFlags.Z, (temp2 & 0x00ff) == 0x00)
      _ <- modifyFlag(CpuFlags.V, (~(a ^ temp1) & (a ^ temp2)) & 0x80)
      _ <- modifyFlag(CpuFlags.N, temp2 & 0x80)
      _ <- setA(temp2 & 0xff)
    } yield ()
   */

  def XXX: Op = identity

  case class Instr(info: String, op: Op, cycles: Int)

  val lookup: Map[UInt8, Instr] = Map(
    0x00 -> Instr("BRK/IMM", BRK, 7),
    0x01 -> Instr("ORA/IMM", ORA(IZX), 6),
    0x05 -> Instr("ORA/ZP0", ORA(ZP0), 3),
    0x06 -> Instr("ASL/ZP0", ASL(ZP0), 5),
    0x08 -> Instr("PHP/IMP", PHP, 3),
    0x09 -> Instr("ORA/IMM", ORA(IMM), 2),
    0x0a -> Instr("ASL/IMP", ASL(IMP), 2),
    0x0d -> Instr("ORA/ABS", ORA(ABS), 4),
    0x0e -> Instr("ASL/ABS", ASL(ABS), 6),
    0x10 -> Instr("BPL/REL", BPL, 2),
    0x11 -> Instr("ORA/IZY", ORA(IZY), 5),
    0x15 -> Instr("ORA/ZPX", ORA(ZPX), 4),
    0x16 -> Instr("ASL/ZPX", ASL(ZPX), 6),
    0x18 -> Instr("CLC/IMP", CLC, 2),
    0x19 -> Instr("ORA/ABY", ORA(ABY), 4),
    0x1d -> Instr("ORA/ABX", ORA(ABX), 4),
    0x1e -> Instr("ASL/ABX", ASL(ABX), 7),
    0x20 -> Instr("JSR/ABS", JSR(ABS), 6),
    0x21 -> Instr("AND/IZX", AND(IZX), 6),
    0x24 -> Instr("BIT/ZP0", BIT(ZP0), 3),
    0x25 -> Instr("AND/ZP0", AND(ZP0), 3),
    0x26 -> Instr("ROL/IMM", ROL(ZP0), 5),
    0x28 -> Instr("PLP/IMP", PLP, 4),
    0x29 -> Instr("AND/IMM", AND(IMM), 2),
    0x2a -> Instr("ROL/IMP", ROL(IMP), 2),
    0x2c -> Instr("BIT/ABS", BIT(ABS), 4),
    0x2d -> Instr("AND/ABS", AND(ABS), 4),
    0x2e -> Instr("ROL/ABS", ROL(ABS), 6),
    0x30 -> Instr("BMI/REL", BMI, 2),
    0x31 -> Instr("AND/IZY", AND(IZY), 5),
    0x35 -> Instr("AND/ZPX", AND(ZPX), 4),
    0x36 -> Instr("ROL/ZPX", ROL(ZPX), 6),
    0x38 -> Instr("SEC/IMP", SEC, 2),
    0x39 -> Instr("AND/ABY", AND(ABY), 4),
    0x3d -> Instr("AND/ABX", AND(ABX), 4),
    0x3e -> Instr("ROL/ABX", ROL(ABX), 7),
    0x40 -> Instr("RTI/IMP", RTI, 6),
    0x41 -> Instr("EOR/IZX", EOR(IZX), 6),
    0x45 -> Instr("EOR/ZP0", EOR(ZP0), 3),
    0x46 -> Instr("LSR/ZP0", LSR(ZP0), 5),
    0x48 -> Instr("PHA/IMP", PHA, 3),
    0x49 -> Instr("EOR/IMM", EOR(IMM), 2),
    0x4a -> Instr("LSR/IMP", LSR(IMP), 2),
    0x4c -> Instr("JMP/ABS", JMP(ABS), 3),
    0x4d -> Instr("EOR/ABS", EOR(ABS), 4),
    0x4e -> Instr("LSR/ABS", LSR(ABS), 6),
    0x50 -> Instr("BVC/REL", BVC, 2),
    0x51 -> Instr("EOR/IZY", EOR(IZY), 5),
    0x55 -> Instr("EOR/ZPX", EOR(ZPX), 4),
    0x56 -> Instr("LSR/ZPX", LSR(ZPX), 6),
    0x58 -> Instr("CLI/IMP", CLI, 2),
    0x59 -> Instr("EOR/ABY", EOR(ABY), 4),
    0x5d -> Instr("EOR/ABX", EOR(ABX), 4),
    0x5e -> Instr("LSR/ABX", LSR(ABX), 7),
    0x60 -> Instr("RTS/IMP", RTS, 6),
    0x61 -> Instr("ADC/IZX", ADC(IZX), 6),
    0x65 -> Instr("ADC/ZP0", ADC(ZP0), 3),
    0x66 -> Instr("ROR/ZP0", ROR(ZP0), 5),
    0x68 -> Instr("PLA/IMP", PLA, 4),
    0x69 -> Instr("ADC/IMM", ADC(IMM), 2),
    0x6a -> Instr("ROR/IMP", ROR(IMP), 2),
    0x6c -> Instr("JMP/IND", JMP(IND), 5),
    0x6d -> Instr("ADC/ABS", ADC(ABS), 4),
    0x6e -> Instr("ROR/ABS", ROR(ABS), 6),
    0x70 -> Instr("BVS/REL", BVS, 2),
    0x71 -> Instr("ADC/IZY", ADC(IZY), 5),
    0x75 -> Instr("ADC/ZPX", ADC(ZPX), 4),
    0x76 -> Instr("ROR/ZPX", ROR(ZPX), 6),
    0x78 -> Instr("SEI/IMP", SEI, 2),
    0x79 -> Instr("ADC/ABY", ADC(ABY), 4),
    0x7d -> Instr("ADC/ABX", ADC(ABX), 4),
    0x7e -> Instr("ROR/ABX", ROR(ABX), 7),
    0x81 -> Instr("STA/IZX", STA(IZX), 6),
    0x84 -> Instr("STY/ZP0", STY(ZP0), 3),
    0x85 -> Instr("STA/ZP0", STA(ZP0), 3),
    0x86 -> Instr("STX/ZP0", STX(ZP0), 3),
    0x88 -> Instr("DEY/IMP", DEY, 2),
    0x8a -> Instr("TXA/IMP", TXA, 2),
    0x8c -> Instr("STY/ABS", STY(ABS), 4),
    0x8d -> Instr("STA/ABS", STA(ABS), 4),
    0x8e -> Instr("STX/ABS", STX(ABS), 4),
    0x90 -> Instr("BCC/REL", BCC, 2),
    0x91 -> Instr("STA/IZY", STA(IZY), 6),
    0x94 -> Instr("STY/ZPX", STY(ZPX), 4),
    0x95 -> Instr("STA/ZPX", STA(ZPX), 4),
    0x96 -> Instr("STX/ZPY", STX(ZPY), 4),
    0x98 -> Instr("TYA/IMP", TYA, 2),
    0x99 -> Instr("STA/ABY", STA(ABY), 5),
    0x9a -> Instr("TXS/IMP", TXS, 2),
    0x9d -> Instr("STA/ABX", STA(ABX), 5),
    0xa0 -> Instr("LDY/IMM", LDY(IMM), 2),
    0xa1 -> Instr("LDA/IZX", LDA(IZX), 6),
    0xa2 -> Instr("LDX/IMM", LDX(IMM), 2),
    0xa4 -> Instr("LDY/ZP0", LDY(ZP0), 3),
    0xa5 -> Instr("LDA/ZP0", LDA(ZP0), 3),
    0xa6 -> Instr("LDX/ZP0", LDX(ZP0), 3),
    0xa8 -> Instr("TAY/IMP", TAY, 2),
    0xa9 -> Instr("LDA/IMM", LDA(IMM), 2),
    0xaa -> Instr("TAX/IMP", TAX, 2),
    0xac -> Instr("LDY/ABS", LDY(ABS), 4),
    0xad -> Instr("LDA/ABS", LDA(ABS), 4),
    0xae -> Instr("LDX/ABS", LDX(ABS), 4),
    0xb0 -> Instr("BCS/REL", BCS, 2),
    0xb1 -> Instr("LDA/IZY", LDA(IZY), 5),
    0xb4 -> Instr("LDY/ZPX", LDY(ZPX), 4),
    0xb5 -> Instr("LDA/ZPX", LDA(ZPX), 4),
    0xb6 -> Instr("LDX/ZPY", LDX(ZPY), 4),
    0xb8 -> Instr("CLV/IMP", CLV, 2),
    0xb9 -> Instr("LDA/ABY", LDA(ABY), 4),
    0xba -> Instr("TSX/IMP", TSX, 2),
    0xbc -> Instr("LDY/ABX", LDY(ABX), 4),
    0xbd -> Instr("LDA/ABX", LDA(ABX), 4),
    0xbe -> Instr("LDX/ABY", LDX(ABY), 4),
    0xc0 -> Instr("CPY/IMM", CPY(IMM), 2),
    0xc1 -> Instr("CMP/IZX", CMP(IZX), 6),
    0xc4 -> Instr("CPY/ZP0", CPY(ZP0), 3),
    0xc5 -> Instr("CMP/ZP0", CMP(ZP0), 3),
    0xc6 -> Instr("DEC/ZP0", DEC(ZP0), 5),
    0xc8 -> Instr("INY/IMP", INY, 2),
    0xc9 -> Instr("CMP/IMM", CMP(IMM), 2),
    0xca -> Instr("DEX/IMP", DEX, 2),
    0xcc -> Instr("CPY/ABS", CPY(ABS), 4),
    0xcd -> Instr("CMP/ABS", CMP(ABS), 4),
    0xce -> Instr("DEC/ABS", DEC(ABS), 6),
    0xd0 -> Instr("BNE/REL", BNE, 2),
    0xd1 -> Instr("CMP/IZY", CMP(IZY), 5),
    0xd5 -> Instr("CMP/ZPX", CMP(ZPX), 4),
    0xd6 -> Instr("DEC/ZPX", DEC(ZPX), 6),
    0xd8 -> Instr("CLD/IMP", CLD, 2),
    0xd9 -> Instr("CMP/ABY", CMP(ABY), 4),
    0xdd -> Instr("CMP/ABX", CMP(ABX), 4),
    0xde -> Instr("DEC/ABX", DEC(ABX), 7),
    0xe0 -> Instr("CPX/IMM", CPX(IMM), 2),
    0xe1 -> Instr("SBC/IZX", SBC(IZX), 6),
    0xe4 -> Instr("CPX/ZP0", CPX(ZP0), 3),
    0xe5 -> Instr("SBC/ZP0", SBC(ZP0), 3),
    0xe6 -> Instr("INC/ZP0", INC(ZP0), 5),
    0xe8 -> Instr("INX/IMP", INX, 2),
    0xe9 -> Instr("SBC/IMM", SBC(IMM), 2),
    0xea -> Instr("NOP/IMP", NOP(IMP), 2),
    0xec -> Instr("CPX/ABS", CPX(ABS), 4),
    0xed -> Instr("SBC/ABS", SBC(ABS), 4),
    0xee -> Instr("INC/ABS", INC(ABS), 6),
    0xf0 -> Instr("BEQ/REL", BEQ, 2),
    0xf1 -> Instr("SBC/IZY", SBC(IZY), 5),
    0xf5 -> Instr("SBC/ZPX", SBC(ZPX), 4),
    0xf6 -> Instr("INC/ZPX", INC(ZPX), 6),
    0xf8 -> Instr("SED/IMP", SED, 2),
    0xf9 -> Instr("SBC/ABY", SBC(ABY), 4),
    0xfd -> Instr("SBC/ABX", SBC(ABX), 4),
    0xfe -> Instr("INC/ABX", INC(ABX), 7)
    // Unofficial opcodes
    /*
    0xa3 -> Instr("LAX/IZX", LAX(IZX), 6),
    0xa7 -> Instr("LAX/ZP0", LAX(ZP0), 3),
    0xab -> Instr("LAX/IMM", LAX(IMM), 2),
    0xaf -> Instr("LAX/ABS", LAX(ABS), 4),
    0xb3 -> Instr("LAX/IZY", LAX(IZY), 5),
    0xb7 -> Instr("LAX/ZPY", LAX(ZPY), 4),
    0xbf -> Instr("LAX/ABY", LAX(ABY), 4),
    0x83 -> Instr("SAX/IZX", SAX(IZX), 6),
    0x87 -> Instr("SAX/ZP0", SAX(ZP0), 3),
    0x8f -> Instr("SAX/ABS", SAX(ABS), 4),
    0x97 -> Instr("SAX/ZPY", SAX(ZPY), 4),
    0xeb -> Instr("SBC/IMM", SBC(IMM), 2),
    0xc3 -> Instr("DCP/IZX", DCP(IZX), 8),
    0xc7 -> Instr("DCP/ZP0", DCP(ZP0), 5),
    0xcf -> Instr("DCP/ABS", DCP(ABS), 6),
    0xd3 -> Instr("DCP/IZY", DCP(IZY), 8),
    0xd7 -> Instr("DCP/ZPX", DCP(ZPX), 6),
    0xdb -> Instr("DCP/ABY", DCP(ABY), 7),
    0xdf -> Instr("DCP/ABX", DCP(ABX), 7),
    0xe3 -> Instr("ISC/IZX", ISC(IZX), 8),
    0xe7 -> Instr("ISC/ZP0", ISC(ZP0), 5),
    0xef -> Instr("ISC/ABS", ISC(ABS), 6),
    0xf3 -> Instr("ISC/IZY", ISC(IZY), 8),
    0xf7 -> Instr("ISC/ZPX", ISC(ZPX), 6),
    0xfb -> Instr("ISC/ABY", ISC(ABY), 7),
    0xff -> Instr("ISC/ABX", ISC(ABX), 7),
    0x03 -> Instr("SLO/IZX", SLO(IZX), 8),
    0x07 -> Instr("SLO/ZP0", SLO(ZP0), 5),
    0x0f -> Instr("SLO/ABS", SLO(ABS), 6),
    0x13 -> Instr("SLO/IZY", SLO(IZY), 8),
    0x17 -> Instr("SLO/ZPX", SLO(ZPX), 6),
    0x1b -> Instr("SLO/ABY", SLO(ABY), 7),
    0x1f -> Instr("SLO/ABX", SLO(ABX), 7),
    0x23 -> Instr("RLA/IZX", RLA(IZX), 8),
    0x27 -> Instr("RLA/ZP0", RLA(ZP0), 5),
    0x2f -> Instr("RLA/ABS", RLA(ABS), 6),
    0x33 -> Instr("RLA/IZY", RLA(IZY), 8),
    0x37 -> Instr("RLA/ZPX", RLA(ZPX), 6),
    0x3b -> Instr("RLA/ABY", RLA(ABY), 7),
    0x3f -> Instr("RLA/ABX", RLA(ABX), 7),
    0x43 -> Instr("SRE/IZX", SRE(IZX), 8),
    0x47 -> Instr("SRE/ZP0", SRE(ZP0), 5),
    0x4f -> Instr("SRE/ABS", SRE(ABS), 6),
    0x53 -> Instr("SRE/IZY", SRE(IZY), 8),
    0x57 -> Instr("SRE/ZPX", SRE(ZPX), 6),
    0x5b -> Instr("SRE/ABY", SRE(ABY), 7),
    0x5f -> Instr("SRE/ABX", SRE(ABX), 7),
    0x63 -> Instr("RRA/IZX", RRA(IZX), 8),
    0x67 -> Instr("RRA/ZP0", RRA(ZP0), 5),
    0x6f -> Instr("RRA/ABS", RRA(ABS), 6),
    0x73 -> Instr("RRA/IZY", RRA(IZY), 8),
    0x77 -> Instr("RRA/ZPX", RRA(ZPX), 6),
    0x7b -> Instr("RRA/ABY", RRA(ABY), 7),
    0x7f -> Instr("RRA/ABX", RRA(ABX), 7)
     */
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

  def disassemble: State[NesState, (UInt16, String)] = {
    def getNext: State[NesState, (UInt16, UInt8)] =
      nes => {
        val pc        = nes.cpuState.pc
        val (nes1, d) = cpuRead(pc)(nes)
        val nes2      = incPc(nes1)
        (nes2, (pc, d))
      }

    def getNextN(n: Int): State[NesState, List[UInt8]] =
      Monad[State[NesState, *]].replicateA(n, getNext).map(_.map(_._2))

    for {
      next <- getNext
      (addr, d)   = next
      infoParts   = lookup(d).info.split('/')
      opcode      = infoParts.head
      addressMode = infoParts.last
      cmdParts <-
        if (addressMode == "IMP") getNextN(0)
        else if (Set("IMM", "ZP0", "ZPX", "ZPY", "IZX", "IZY", "REL").contains(addressMode))
          getNextN(1)
        else if (Set("ABS", "ABX", "ABY", "IND").contains(addressMode)) getNextN(2)
        else throw new RuntimeException(s"Unexpected address mode: $addressMode")
      res = addressMode match {
        case "IMP" =>
          s"$opcode {IMP}"
        case "IMM" =>
          s"$opcode #$$${hex(cmdParts.head, 2)} {IMM}"
        case "ZP0" =>
          s"$opcode $$${hex(cmdParts.head, 2)} {ZP0}"
        case "ZPX" =>
          s"$opcode $$${hex(cmdParts.head, 2)}, X {ZPX}"
        case "ZPY" =>
          s"$opcode $$${hex(cmdParts.head, 2)}, Y {ZPY}"
        case "IZX" =>
          s"$opcode ($$${hex(cmdParts.head, 2)}, X) {IZX}"
        case "IZY" =>
          s"$opcode ($$${hex(cmdParts.head, 2)}, Y) {IZY}"
        case "ABS" =>
          val a = asUInt16(cmdParts(1), cmdParts.head)
          s"$opcode $$${hex(a, 4)} {ABS}"
        case "ABX" =>
          val a = asUInt16(cmdParts(1), cmdParts.head)
          s"$opcode $$${hex(a, 4)}, X {ABX}"
        case "ABY" =>
          val a = asUInt16(cmdParts(1), cmdParts.head)
          s"$opcode $$${hex(a, 4)}, Y {ABY}"
        case "IND" =>
          val a = asUInt16(cmdParts(1), cmdParts.head)
          s"$opcode ($$${hex(a, 4)}) {IND}"
        case "REL" =>
          val a = cmdParts.head
          s"$opcode $$${hex(a, 2)} [$$${hex(addr + 2 + a.toByte, 4)}] {REL}"
        case _ =>
          throw new RuntimeException(s"Unexpected address mode: $addressMode")
      }
    } yield addr -> res
  }

  def disassemble(n: Int): State[NesState, List[(UInt16, String)]] =
    Monad[State[NesState, *]].replicateA(n, disassemble)

}
