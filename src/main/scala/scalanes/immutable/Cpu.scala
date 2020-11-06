package scalanes.immutable

import cats.Monad
import cats.implicits._
import com.typesafe.scalalogging.LazyLogging
import monocle.Lens
import monocle.macros.GenLens
import scalanes.immutable.CpuFlags.CpuFlags

case class CpuState(a: UInt8, x: UInt8, y: UInt8, stkp: UInt8, pc: UInt16, status: UInt8, cycles: Int, haltAt: UInt16) {

  require((a & 0xff) == a)
  require((x & 0xff) == x)
  require((y & 0xff) == y)
  require((stkp & 0xff) == stkp)
  require((status & 0xff) == status)

  def getFlag(flag: CpuFlags): Boolean = status & flag.bit
}

object CpuState {
  val a: Lens[CpuState, UInt8]       = GenLens[CpuState](_.a)
  val x: Lens[CpuState, UInt8]       = GenLens[CpuState](_.x)
  val y: Lens[CpuState, UInt8]       = GenLens[CpuState](_.y)
  val stkp: Lens[CpuState, UInt8]    = GenLens[CpuState](_.stkp)
  val pc: Lens[CpuState, UInt16]     = GenLens[CpuState](_.pc)
  val status: Lens[CpuState, UInt8]  = GenLens[CpuState](_.status)
  val cycles: Lens[CpuState, Int]    = GenLens[CpuState](_.cycles)
  val haltAt: Lens[CpuState, UInt16] = GenLens[CpuState](_.haltAt)

  val initial: CpuState =
    CpuState(0x00, 0x00, 0x00, 0xfd, 0x0000, 0x00 | CpuFlags.U.bit | CpuFlags.I.bit, 0, 0xffff)
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

object Cpu extends LazyLogging {

  sealed trait Address {
    def read(): State[NesState, UInt8]
    def write(d: UInt8): State[NesState, Unit]
  }

  case class AbsAddress(address: UInt16) extends Address {
    require((address & 0xffff) == address)
    override def read(): State[NesState, UInt8]         = cpuRead(address)
    override def write(d: UInt8): State[NesState, Unit] = cpuWrite(address, d)
  }

  case object AccAddress extends Address {
    override def read(): State[NesState, UInt8]         = getA
    override def write(d: UInt8): State[NesState, Unit] = setA(d)
  }

  type AddressMode    = State[NesState, _ <: Address]
  type AbsAddressMode = State[NesState, AbsAddress]
  type AccAddressMode = State[NesState, AccAddress.type]
  type RelAddressMode = State[NesState, Byte]
  type Op             = State[NesState, Unit]

  implicit class CpuStateOps[A](val a: State[CpuState, A]) extends AnyVal {
    def toNesState: State[NesState, A] = a.transformS(
      NesState.cpuState.get,
      (nesState, cpuState) => NesState.cpuState.set(cpuState)(nesState)
    )
  }

  private def readExecute(addressMode: AddressMode)(op: (UInt8, CpuState) => CpuState): Op = for {
    address <- addressMode
    d       <- address.read()
    _       <- State.modify(NesState.cpuState.modify(op(d, _)))
  } yield ()

  private def readExecuteWrite(addressMode: AddressMode)(op: (UInt8, CpuState) => (CpuState, UInt8)): Op = for {
    address <- addressMode
    dIn     <- address.read()
    dOut    <- State(op(dIn, _)).toNesState
    _       <- address.write(dOut)
  } yield ()

  private def executeWrite(addressMode: AddressMode)(op: CpuState => (CpuState, UInt8)): Op = for {
    address <- addressMode
    dOut    <- State(op).toNesState
    _       <- address.write(dOut)
  } yield ()

  private def lift(f: CpuState => CpuState): NesState => NesState =
    NesState.cpuState.modify(f)

  private def liftS(f: CpuState => CpuState): State[NesState, Unit] =
    State.modify(lift(f))

  val incPc: State[NesState, UInt16] = State { nes =>
    val updated = lift(CpuState.pc.modify(pc => (pc + 1) & 0xffff))(nes)
    (updated, updated.cpuState.pc)
  }

  val decPc: State[NesState, UInt16] = State { nes =>
    val updated = lift(CpuState.pc.modify(pc => (pc - 1) & 0xffff))(nes)
    (updated, updated.cpuState.pc)
  }

  val getPc: State[NesState, UInt16] = State.inspect(_.cpuState.pc)

  def setPc(d: UInt16): State[NesState, Unit] = {
    require((d & 0xffff) == d)
    liftS(CpuState.pc.set(d))
  }

  val getX: State[NesState, UInt8] = State.inspect(_.cpuState.x)

  def setX(d: UInt8): State[NesState, Unit] = {
    require((d & 0xff) == d)
    liftS(CpuState.x.set(d))
  }

  val getY: State[NesState, UInt8] = State.inspect(_.cpuState.y)

  def setY(d: UInt8): State[NesState, Unit] = {
    require((d & 0xff) == d)
    liftS(CpuState.y.set(d))
  }

  val getA: State[NesState, UInt8] = State.inspect(_.cpuState.a)

  def setA(d: UInt8): State[NesState, Unit] = liftS(CpuState.a.set(d))

  val getStkp: State[NesState, UInt8] = State.inspect(_.cpuState.stkp)

  def setStkp(d: UInt8): State[NesState, Unit] = liftS(CpuState.stkp.set(d))

  val decStkp: State[NesState, UInt8] = State { nes =>
    val updated = lift(CpuState.stkp.modify(stkp => (stkp - 1) & 0xff))(nes)
    (updated, updated.cpuState.stkp)
  }

  val incStkp: State[NesState, UInt8] = State { nes =>
    val updated = lift(CpuState.stkp.modify(stkp => (stkp + 1) & 0xff))(nes)
    (updated, updated.cpuState.stkp)
  }

  val getCycles: State[NesState, Int] = State.inspect(_.cpuState.cycles)

  def setCycles(d: Int): State[NesState, Unit] =
    liftS(CpuState.cycles.set(d))

  def incCycles(n: Int): State[NesState, Int] = State { nes =>
    val updated = lift(CpuState.cycles.modify(_ + n))(nes)
    (updated, updated.cpuState.cycles)
  }

  def setHaltAt(address: UInt16): State[NesState, Unit] =
    liftS(CpuState.haltAt.set(address))

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

  def fastCpuRead(address: UInt16)(nes: NesState): UInt8 = {
    require((address & 0xffff) == address)

    if (address >= 0x0000 && address <= 0x1fff) // RAM
      nes.ram(address % 0x800)
    else if (address >= 0x6000 && address <= 0xffff) // Cartridge
      Cartridge.fastCpuRead(address)(nes)
    else
      throw new RuntimeException(s"Invalid address $address for fast cpu read.")
  }

  def cpuWrite(address: UInt16, d: UInt8): State[NesState, Unit] = {
    require((address & 0xffff) == address)
    require((d & 0xff) == d)

    if (address >= 0x0000 && address <= 0x1fff) //RAM
      State.modify(NesState.ram.modify(_.updated(address, d)))
    else if (address >= 0x2000 && address <= 0x3fff) // PPU registers
      Ppu.cpuWrite(address, d)
    else if (address == 0x4014) { // OAM DMA
      val page = d << 8
      incCycles(513) *> (0 until 256)
        .map { oamAddress =>
          cpuRead(page | oamAddress).flatMap(Ppu.writeOam(oamAddress, _))
        }
        .reduce(_ *> _)
    } else if (address == 0x4016 && (d & 0x01)) // Controller 1
      Controller.writeController1
    else if (address == 0x4017 && (d & 0x01)) // Controller 2
      Controller.writeController2
    else if (address >= 0x6000 && address <= 0xffff) // Cartridge
      Cartridge.cpuWrite(address, d)
    else
      State.pure(())
  }

  def fastCpuWrite(address: UInt16, d: UInt8)(nes: NesState): NesState = {
    require((address & 0xffff) == address)
    require((d & 0xff) == d)

    if (address >= 0x0000 && address <= 0x1fff) //RAM
      NesState.ram.modify(_.updated(address, d))(nes)
    else
      throw new RuntimeException(s"Invalid address $address for fast cpu write.")
  }

  val getStatus: State[NesState, UInt8] = State.inspect(_.cpuState.status)

  def setStatus(d: UInt8): State[NesState, Unit] = liftS(CpuState.status.set(d))

  def setFlag(flag: CpuFlags, value: Boolean): State[NesState, Unit] =
    liftS(CpuState.status.modify(s => if (value) s | flag.bit else s & ~flag.bit))

  def setFlags(flags: (CpuFlags, Boolean)*)(s: UInt8): UInt8 =
    flags.foldLeft(s) { case (acc, (f, v)) => if (v) acc | f.bit else acc & ~f.bit }

  def setZnFlags(d: UInt8)(s: UInt8): UInt8 =
    setFlags((CpuFlags.Z, d == 0x00), (CpuFlags.N, d & 0x80))(s)

  def getFlag(flag: CpuFlags): State[NesState, Boolean] = State.inspect(flag.bit & _.cpuState.status)

  val pop: State[NesState, UInt8] = State { nes =>
    val updated = lift(
      CpuState.stkp.modify(stkp => (stkp + 1) & 0xff)
    )(nes)
    val address = (0x0100 + updated.cpuState.stkp) & 0xffff
    (updated, fastCpuRead(address)(nes))
  }

  def push(d: UInt8): State[NesState, Unit] = State.modify { nes =>
    val address = (0x0100 + nes.cpuState.stkp) & 0xffff
    val update =
      fastCpuWrite(address, d) _ andThen
        lift(CpuState.stkp.modify(stkp => (stkp - 1) & 0xff))
    update(nes)
  }

  private def isPageChange(a: Int, i: Int): Boolean = ((a + i) & 0xff00) != (a & 0xff00)

  private def asUInt16(hi: UInt8, lo: UInt8): UInt16 = {
    require((hi & 0xff) == hi)
    require((lo & 0xff) == lo)

    (hi << 8) | lo
  }

  def reset: State[NesState, Unit] = for {
    lo <- cpuRead(0xfffc)
    hi <- cpuRead(0xfffd)
    _  <- setPc(asUInt16(hi, lo))
    _  <- setA(0)
    _  <- setX(0)
    _  <- setY(0)
    _  <- setStkp(0xfd)
    _  <- setStatus(0x00 | CpuFlags.U.bit | CpuFlags.I.bit)
    _  <- setCycles(0)
    _  <- setHaltAt(0xffff)
  } yield ()

  def irq: State[NesState, Unit] = for {
    pc <- getPc
    pcHi = (pc >> 8) & 0xff
    _ <- push(pcHi)
    pcLo = pc & 0xff
    _      <- push(pcLo)
    _      <- setFlag(CpuFlags.B, value = false)
    _      <- setFlag(CpuFlags.U, value = true)
    _      <- setFlag(CpuFlags.I, value = true)
    status <- getStatus
    _      <- push(status)
    lo     <- cpuRead(0xfffe)
    hi     <- cpuRead(0xfffe + 1)
    _      <- setPc(asUInt16(hi, lo))
    _      <- setCycles(7)
  } yield ()

  def nmi: State[NesState, NesState] = for {
    pc <- getPc
    pcHi = (pc >> 8) & 0xff
    _ <- push(pcHi)
    pcLo = pc & 0xff
    _      <- push(pcLo)
    _      <- setFlag(CpuFlags.B, value = false)
    _      <- setFlag(CpuFlags.U, value = true)
    _      <- setFlag(CpuFlags.I, value = true)
    status <- getStatus
    _      <- push(status)
    lo     <- cpuRead(0xfffa)
    hi     <- cpuRead(0xfffa + 1)
    _      <- setPc(asUInt16(hi, lo))
    _      <- setCycles(8)
    s      <- State.get
  } yield s

  val clock: State[NesState, NesState] = State.get.flatMap { nes =>
    if (nes.cpuState.cycles == 0 && nes.cpuState.haltAt == nes.cpuState.pc)
      State { nes =>
        val updated = lift(
          CpuState.cycles.set(10)
        )(nes)
        (updated, updated)
      }
    else if (nes.cpuState.cycles == 0) {
      val opCode = fastCpuRead(nes.cpuState.pc)(nes)
      val instr  = lookup(opCode)
      State[NesState, Unit] { nes =>
        val updated = lift(
          CpuState.cycles.set(instr.cycles) andThen
            CpuState.pc.modify(_ + 1)
        )(nes)
        (updated, ())
      } *> instr.op *> State { nes =>
        val updated = lift(
          CpuState.status.modify(setFlags(CpuFlags.U -> true))
        )(nes)
        (updated, updated)
      }
    } else
      State { nes =>
        val updated = lift(
          CpuState.cycles.modify(_ - 1)
        )(nes)
        (updated, updated)
      }
  }

  val executeNextInstr: State[NesState, Unit] = for {
    _ <- clock
    _ <- Monad[State[NesState, *]].whileM_(getCycles.map(_ != 0))(clock)
  } yield ()

  // Implicit
  // It may operate on the accumulator.
  val IMP: AccAddressMode = State.pure(AccAddress)

  // Immediate - #v
  // Uses the 8-bit operand itself as the value for the operation, rather than fetching a value from a memory address.
  val IMM: AbsAddressMode = State { nes =>
    val pc      = nes.cpuState.pc
    val updated = lift(CpuState.pc.modify(_ + 1))(nes)
    (updated, AbsAddress(pc))
  }

  // Zero page - d
  // Fetches the value from an 8-bit address on the zero page.
  val ZP0: AbsAddressMode = State { nes =>
    val pc      = nes.cpuState.pc
    val address = fastCpuRead(pc)(nes)
    val updated = lift(CpuState.pc.modify(_ + 1))(nes)
    (updated, AbsAddress(address))
  }

  // Zero page indexed - d,x
  val ZPX: AbsAddressMode = State { nes =>
    val pc      = nes.cpuState.pc
    val address = (fastCpuRead(pc)(nes) + nes.cpuState.x) & 0xff
    val updated = lift(CpuState.pc.modify(_ + 1))(nes)
    (updated, AbsAddress(address))
  }

  // Zero page indexed - d,y
  val ZPY: AbsAddressMode = State { nes =>
    val pc      = nes.cpuState.pc
    val address = (fastCpuRead(pc)(nes) + nes.cpuState.y) & 0xff
    val updated = lift(CpuState.pc.modify(_ + 1))(nes)
    (updated, AbsAddress(address))
  }

  val REL: RelAddressMode = State { nes =>
    val pc         = nes.cpuState.pc
    val updated    = lift(CpuState.pc.modify(_ + 1))(nes)
    val relAddress = fastCpuRead(pc)(nes).toByte
    (updated, relAddress)
  }

  // Absolute - a
  // Fetches the value from a 16-bit address anywhere in memory.
  val ABS: AbsAddressMode = State { nes =>
    val pc      = nes.cpuState.pc
    val lo      = fastCpuRead(pc)(nes)
    val hi      = fastCpuRead(pc + 1)(nes)
    val address = asUInt16(hi, lo)
    val updated = lift(CpuState.pc.modify(_ + 2))(nes)
    (updated, AbsAddress(address))
  }

  // Absolute indexed - a,x
  val ABX: AbsAddressMode = State { nes =>
    val pc      = nes.cpuState.pc
    val lo      = fastCpuRead(pc)(nes)
    val hi      = fastCpuRead(pc + 1)(nes)
    val address = asUInt16(hi, lo)
    val c       = if (isPageChange(address, nes.cpuState.x)) 1 else 0
    val updated = lift(
      CpuState.pc.modify(_ + 2) andThen
        CpuState.cycles.modify(_ + c)
    )(nes)
    (updated, AbsAddress((address + nes.cpuState.x) & 0xffff))
  }

  // Absolute indexed - a,y
  val ABY: AbsAddressMode = State { nes =>
    val pc      = nes.cpuState.pc
    val lo      = fastCpuRead(pc)(nes)
    val hi      = fastCpuRead(pc + 1)(nes)
    val address = asUInt16(hi, lo)
    val c       = if (isPageChange(address, nes.cpuState.y)) 1 else 0
    val updated = lift(
      CpuState.pc.modify(_ + 2) andThen
        CpuState.cycles.modify(_ + c)
    )(nes)
    (updated, AbsAddress((address + nes.cpuState.y) & 0xffff))
  }

  // Indirect - (a)
  // The JMP instruction has a special indirect addressing mode that can jump to the address stored in a
  // 16-bit pointer anywhere in memory.
  val IND: AbsAddressMode = State { nes =>
    val pc    = nes.cpuState.pc
    val ptrLo = fastCpuRead(pc)(nes)
    val ptrHi = fastCpuRead(pc + 1)(nes)
    val ptr   = asUInt16(ptrHi, ptrLo)
    val lo    = fastCpuRead(ptr)(nes)
    val hi =
      if ((ptr & 0x00ff) == 0x00ff)
        fastCpuRead(ptr & 0xff00)(nes)
      else
        fastCpuRead((ptr + 1) & 0xffff)(nes)
    val address = asUInt16(hi, lo)
    val updated = lift(CpuState.pc.modify(_ + 2))(nes)
    (updated, AbsAddress(address))
  }

  // Indexed indirect - (d,x)
  val IZX: AbsAddressMode = State { nes =>
    val pc      = nes.cpuState.pc
    val t       = fastCpuRead(pc)(nes)
    val x       = nes.cpuState.x
    val lo      = fastCpuRead((t + x + 0) & 0x00ff)(nes)
    val hi      = fastCpuRead((t + x + 1) & 0x00ff)(nes)
    val address = asUInt16(hi, lo)
    val updated = lift(CpuState.pc.modify(_ + 1))(nes)
    (updated, AbsAddress(address))
  }

  // Indirect indexed - (d),y
  val IZY: AbsAddressMode = State { nes =>
    val pc      = nes.cpuState.pc
    val t       = fastCpuRead(pc)(nes)
    val y       = nes.cpuState.y
    val lo      = fastCpuRead((t + 0) & 0x00ff)(nes)
    val hi      = fastCpuRead((t + 1) & 0x00ff)(nes)
    val address = asUInt16(hi, lo)
    val c       = if (isPageChange(address, y)) 1 else 0
    val updated = lift(
      CpuState.cycles.modify(_ + c) andThen
        CpuState.pc.modify(_ + 1)
    )(nes)
    (updated, AbsAddress((address + y) & 0xffff))
  }

  // Add with carry
  def ADC(addressMode: AddressMode): Op = readExecute(addressMode) { (d, cpu) =>
    val c    = cpu.getFlag(CpuFlags.C)
    val lsb  = if (c) 1 else 0
    val temp = cpu.a + d + lsb
    val update =
      CpuState.a.set(temp & 0xff) andThen
        CpuState.status.modify(
          setFlags(
            CpuFlags.C -> (temp & 0xff00),
            CpuFlags.Z -> ((temp & 0x00ff) == 0x00),
            CpuFlags.V -> ((~(cpu.a ^ d) & (cpu.a ^ temp)) & 0x80),
            CpuFlags.N -> (temp & 0x80)
          )
        )
    update(cpu)
  }

  // Logical AND
  def AND(addressMode: AddressMode): Op = readExecute(addressMode) { (d, cpu) =>
    val r = cpu.a & d & 0xff
    val update =
      CpuState.a.set(r) andThen
        CpuState.status.modify(setZnFlags(r))
    update(cpu)
  }

  // Arithmetic shift left
  def ASL(addressMode: AddressMode): Op = readExecuteWrite(addressMode) { (d, cpu) =>
    val temp = d << 1
    val update = CpuState.status.modify(
      setFlags(
        CpuFlags.C -> (temp & 0xff00),
        CpuFlags.Z -> ((temp & 0x00ff) == 0x00),
        CpuFlags.N -> (temp & 0x80)
      )
    )
    (update(cpu), temp & 0xff)
  }

  def branchIf(p: NesState => Boolean): Op = State.get[NesState].flatMap { nes =>
    if (p(nes))
      REL.transform { (nes, relAddress) =>
        val cpu        = nes.cpuState
        val absAddress = (cpu.pc + relAddress) & 0xffff
        val cycles     = cpu.cycles + (if (isPageChange(cpu.pc, relAddress)) 2 else 1)
        val updated    = cpu.copy(pc = absAddress, cycles = cycles)
        (nes.copy(cpuState = updated), ())
      }
    else {
      val cpu     = nes.cpuState
      val updated = nes.copy(cpuState = cpu.copy(pc = cpu.pc + 1))
      State.set(updated)
    }
  }

  // Branch if carry clear
  val BCC: Op = branchIf(!_.cpuState.getFlag(CpuFlags.C))

  // Branch if carry set
  val BCS: Op = branchIf(_.cpuState.getFlag(CpuFlags.C))

  // Branch if equal
  val BEQ: Op = branchIf(_.cpuState.getFlag(CpuFlags.Z))

  // Bit test
  def BIT(addressMode: AddressMode): Op = readExecute(addressMode) { (d, cpu) =>
    val temp = cpu.a & d
    val update = CpuState.status.modify(
      setFlags(
        CpuFlags.Z -> ((temp & 0x00ff) == 0x00),
        CpuFlags.N -> (d & (1 << 7)),
        CpuFlags.V -> (d & (1 << 6))
      )
    )
    update(cpu)
  }

  // Branch if minus
  val BMI: Op = branchIf(_.cpuState.getFlag(CpuFlags.N))

  // Branch if not equal
  val BNE: Op = branchIf(!_.cpuState.getFlag(CpuFlags.Z))

  // Branch if positive
  val BPL: Op = branchIf(!_.cpuState.getFlag(CpuFlags.N))

  // Force interrupt
  def BRK: Op = for {
    pc1    <- incPc
    _      <- setFlag(CpuFlags.I, value = true)
    _      <- push((pc1 >> 8) & 0xff)
    _      <- push(pc1 & 0xff)
    _      <- setFlag(CpuFlags.B, value = true)
    status <- getStatus
    _      <- push(status)
    _      <- setFlag(CpuFlags.B, value = false)
    d1     <- cpuRead(0xfffe)
    d2     <- cpuRead(0xffff)
    pc2 = asUInt16(d2, d1)
    _ <- setPc(pc2)
  } yield ()

  // Branch if overflow clear
  val BVC: Op = branchIf(!_.cpuState.getFlag(CpuFlags.V))

  // Branch if overflow set
  val BVS: Op = branchIf(_.cpuState.getFlag(CpuFlags.V))

  // Clear carry flag
  val CLC: Op = setFlag(CpuFlags.C, value = false)

  // Clear decimal mode
  val CLD: Op = setFlag(CpuFlags.D, value = false)

  // Clear interrupt disable
  val CLI: Op = setFlag(CpuFlags.I, value = false)

  // Clear overflow flag
  val CLV: Op = setFlag(CpuFlags.V, value = false)

  def compareS(getter1: State[NesState, UInt8], getter2: State[NesState, UInt8]): Op = for {
    d1 <- getter1
    d2 <- getter2
    temp = d2 - d1
    _ <- setFlag(CpuFlags.C, d2 >= d1)
    _ <- setFlag(CpuFlags.Z, (temp & 0x00ff) == 0x0000)
    _ <- setFlag(CpuFlags.N, temp & 0x0080)
  } yield ()

  def compare(getter: CpuState => UInt8): (UInt8, CpuState) => CpuState = { (d1, cpu) =>
    val d2   = getter(cpu)
    val temp = d2 - d1
    val update = CpuState.status.modify(
      setFlags(
        CpuFlags.C -> (d2 >= d1),
        CpuFlags.Z -> ((temp & 0x00ff) == 0x0000),
        CpuFlags.N -> (temp & 0x0080)
      )
    )
    update(cpu)
  }

  // Compare
  def CMP(addressMode: AddressMode): Op = readExecute(addressMode)(compare(_.a))

  // Compare X register
  def CPX(addressMode: AddressMode): Op = readExecute(addressMode)(compare(_.x))

  // Compare Y register
  def CPY(addressMode: AddressMode): Op = readExecute(addressMode)(compare(_.y))

  // Decrement memory
  def DEC(addressMode: AddressMode): Op = readExecuteWrite(addressMode) { (d, cpu) =>
    val temp   = (d - 1) & 0xff
    val update = CpuState.status.modify(setZnFlags(temp))
    (update(cpu), temp)
  }

  // Decrement X register
  val DEX: Op = liftS { cpu =>
    val temp = (cpu.x - 1) & 0xff
    val update =
      CpuState.x.set(temp) andThen
        CpuState.status.modify(setZnFlags(temp))
    update(cpu)
  }

  // Decrement Y register
  val DEY: Op = liftS { cpu =>
    val temp = (cpu.y - 1) & 0xff
    val update =
      CpuState.y.set(temp) andThen
        CpuState.status.modify(setZnFlags(temp))
    update(cpu)
  }

  // Exclusive OR
  def EOR(addressMode: AddressMode): Op = readExecute(addressMode) { (d, cpu) =>
    val temp = (cpu.a ^ d) & 0xff
    val update =
      CpuState.a.set(temp) andThen
        CpuState.status.modify(setZnFlags(temp))
    update(cpu)
  }

  // Increment memory
  def INC(addressMode: AddressMode): Op = readExecuteWrite(addressMode) { (d, cpu) =>
    val temp   = (d + 1) & 0xff
    val update = CpuState.status.modify(setZnFlags(temp))
    (update(cpu), temp)
  }

  // Increment X register
  val INX: Op = liftS { cpu =>
    val temp = (cpu.x + 1) & 0xff
    val update =
      CpuState.x.set(temp) andThen
        CpuState.status.modify(setZnFlags(temp))
    update(cpu)
  }

  // Increment Y register
  val INY: Op = liftS { cpu =>
    val temp = (cpu.y + 1) & 0xff
    val update =
      CpuState.y.set(temp) andThen
        CpuState.status.modify(setZnFlags(temp))
    update(cpu)
  }

  // Jump
  def JMP(addressMode: AbsAddressMode): Op = for {
    pc      <- getPc
    address <- addressMode
    _       <- setPc(address.address)
    _ <-
      if (pc == address.address + 1)
        // Detect a jump to itself
        setHaltAt(address.address)
      else
        State.get[NesState]
  } yield ()

  // Jump to subroutine
  def JSR(addressMode: AbsAddressMode): Op = for {
    address <- addressMode
    pc      <- decPc
    _       <- push((pc >> 8) & 0xff)
    _       <- push(pc & 0xff)
    _       <- setPc(address.address)
  } yield ()

  // Load accumulator
  def LDA(addressMode: AddressMode): Op = readExecute(addressMode) { (d, cpu) =>
    val update =
      CpuState.a.set(d) andThen
        CpuState.status.modify(setZnFlags(d))
    update(cpu)
  }

  // Load X register
  def LDX(addressMode: AddressMode): Op = readExecute(addressMode) { (d, cpu) =>
    val update =
      CpuState.x.set(d) andThen
        CpuState.status.modify(setZnFlags(d))
    update(cpu)
  }

  // Load Y register
  def LDY(addressMode: AddressMode): Op = readExecute(addressMode) { (d, cpu) =>
    val update =
      CpuState.y.set(d) andThen
        CpuState.status.modify(setZnFlags(d))
    update(cpu)
  }

  // Logical shift right
  def LSR(addressMode: AddressMode): Op = readExecuteWrite(addressMode) { (d, cpu) =>
    val temp = (d >> 1) & 0xff
    val update = CpuState.status.modify(
      setFlags(
        CpuFlags.C -> (d & 0x01),
        CpuFlags.Z -> ((temp & 0xff) == 0x00),
        CpuFlags.N -> (temp & 0x80)
      )
    )
    (update(cpu), temp)
  }

  // No operation
  def NOP(addressMode: AddressMode): Op = for {
    address <- addressMode
    _       <- address.read()
  } yield ()

  // Logical inclusive OR
  def ORA(addressMode: AddressMode): Op = readExecute(addressMode) { (d, cpu) =>
    val temp = (cpu.a | d) & 0xff
    val update =
      CpuState.a.set(temp) andThen
        CpuState.status.modify(setZnFlags(temp))
    update(cpu)
  }

  // Push accumulator
  val PHA: Op = for {
    a <- getA
    _ <- push(a)
  } yield ()

  // Push processor status
  val PHP: Op = for {
    status <- getStatus
    _      <- push(status | CpuFlags.B.bit | CpuFlags.U.bit)
    _      <- setFlag(CpuFlags.B, value = false)
    _      <- setFlag(CpuFlags.U, value = false)
  } yield ()

  // Pull accumulator
  val PLA: Op = for {
    d <- pop
    _ <- setA(d)
    _ <- setFlag(CpuFlags.Z, d == 0x00)
    _ <- setFlag(CpuFlags.N, d & 0x80)
  } yield ()

  // Pull processor status
  val PLP: Op = for {
    status <- pop
    _      <- setStatus(status)
    _      <- setFlag(CpuFlags.U, value = true)
  } yield ()

  // Rotate left
  def ROL(addressMode: AddressMode): Op = readExecuteWrite(addressMode) { (d, cpu) =>
    val c    = cpu.getFlag(CpuFlags.C)
    val lsb  = if (c) 1 else 0
    val temp = (d << 1) | lsb
    val update = CpuState.status.modify(
      setFlags(
        CpuFlags.C -> (temp & 0xff00),
        CpuFlags.Z -> ((temp & 0x00ff) == 0x00),
        CpuFlags.N -> (temp & 0x80)
      )
    )
    (update(cpu), temp & 0xff)
  }

  // Rotate right
  def ROR(addressMode: AddressMode): Op = readExecuteWrite(addressMode) { (d, cpu) =>
    val c    = cpu.getFlag(CpuFlags.C)
    val msb  = if (c) 1 << 7 else 0
    val temp = (d >> 1) | msb
    val update = CpuState.status.modify(
      setFlags(
        CpuFlags.C -> (d & 0x01),
        CpuFlags.Z -> ((temp & 0x00ff) == 0x00),
        CpuFlags.N -> (temp & 0x80)
      )
    )
    (update(cpu), temp & 0xff)
  }

  // Return from interrupt
  val RTI: Op = for {
    status1 <- pop
    status2 = status1 & ~CpuFlags.B.bit & ~CpuFlags.U.bit
    _   <- setStatus(status2)
    pc1 <- pop
    pc2 <- pop
    pc = asUInt16(pc2, pc1)
    _ <- setPc(pc)
  } yield ()

  // Return from subroutine
  val RTS: Op = for {
    pc1 <- pop
    pc2 <- pop
    pc = asUInt16(pc2, pc1)
    _ <- setPc(pc)
    _ <- incPc
  } yield ()

  // Subtract with carry
  def SBC(addressMode: AddressMode): Op = readExecute(addressMode) { (d, cpu) =>
    val value = d ^ 0x00ff
    val c     = cpu.getFlag(CpuFlags.C)
    val lsb   = if (c) 1 else 0
    val temp  = cpu.a + value + lsb
    val update =
      CpuState.a.set(temp & 0xff) andThen
        CpuState.status.modify(
          setFlags(
            (CpuFlags.C, temp & 0xff00),
            (CpuFlags.Z, (temp & 0x00ff) == 0x00),
            (CpuFlags.V, (temp ^ cpu.a) & (temp ^ value) & 0x80),
            (CpuFlags.N, temp & 0x80)
          )
        )
    update(cpu)
  }

  // Set carry flag
  val SEC: Op = setFlag(CpuFlags.C, value = true)

  // Set decimal flag
  val SED: Op = setFlag(CpuFlags.D, value = true)

  // Set interrupt disable
  val SEI: Op = setFlag(CpuFlags.I, value = true)

  // Store accumulator
  def STA(addressMode: AddressMode): Op = executeWrite(addressMode) { cpu =>
    (cpu, cpu.a)
  }

  // Store X register
  def STX(addressMode: AddressMode): Op = executeWrite(addressMode) { cpu =>
    (cpu, cpu.x)
  }

  // Store Y register
  def STY(addressMode: AddressMode): Op = executeWrite(addressMode) { cpu =>
    (cpu, cpu.y)
  }

  // Transfer accumulator to X
  val TAX: Op = liftS { cpu =>
    val update =
      CpuState.x.set(cpu.a) andThen
        CpuState.status.modify(setZnFlags(cpu.a))
    update(cpu)
  }

  // Transfer accumulator to Y
  val TAY: Op = liftS { cpu =>
    val update =
      CpuState.y.set(cpu.a) andThen
        CpuState.status.modify(setZnFlags(cpu.a))
    update(cpu)
  }

  // Transfer stack pointer to X
  val TSX: Op = liftS { cpu =>
    val update =
      CpuState.x.set(cpu.stkp) andThen
        CpuState.status.modify(setZnFlags(cpu.stkp))
    update(cpu)
  }

  // Transfer X to accumulator
  val TXA: Op = liftS { cpu =>
    val update =
      CpuState.a.set(cpu.x) andThen
        CpuState.status.modify(setZnFlags(cpu.x))
    update(cpu)
  }

  // Transfer X to stack pointer
  val TXS: Op = liftS { cpu =>
    val update = CpuState.stkp.set(cpu.x)
    update(cpu)
  }

  // Transfer Y to accumulator
  val TYA: Op = liftS { cpu =>
    val update =
      CpuState.a.set(cpu.y) andThen
        CpuState.status.modify(setZnFlags(cpu.y))
    update(cpu)
  }

  // *** Unofficial instructions ***

  // Shortcut for LDA value then TAX
  def LAX(addressMode: AddressMode): Op = for {
    _ <- LDA(addressMode)
    _ <- TAX
  } yield ()

  // Stores the bitwise AND of A and X. As with STA and STX, no flags are affected.
  def SAX(addressMode: AddressMode): Op = for {
    address <- addressMode
    a       <- getA
    x       <- getX
    d = a & x
    _ <- address.write(d)
  } yield ()

  // Equivalent to DEC value then CMP value
  def DCP(addressMode: AddressMode): Op = for {
    address <- addressMode
    d       <- address.read()
    temp = (d - 1) & 0xff
    _ <- address.write(temp)
    _ <- setFlag(CpuFlags.Z, temp == 0x00)
    _ <- setFlag(CpuFlags.N, temp & 0x80)
    _ <- compareS(address.read(), getA)
  } yield ()

  // Equivalent to INC value then SBC value
  def ISC(addressMode: AddressMode): Op = for {
    address <- addressMode
    d       <- address.read()
    temp1 = (d + 1) & 0xff
    _ <- address.write(temp1)
    value = temp1 ^ 0x00ff
    a <- getA
    c <- getFlag(CpuFlags.C)
    lsb   = if (c) 1 else 0
    temp2 = a + value + lsb
    _ <- setFlag(CpuFlags.C, temp2 & 0xff00)
    _ <- setFlag(CpuFlags.Z, (temp2 & 0x00ff) == 0x00)
    _ <- setFlag(CpuFlags.V, (temp2 ^ a) & (temp2 ^ value) & 0x80)
    _ <- setFlag(CpuFlags.N, temp2 & 0x80)
    _ <- setA(temp2 & 0xff)
  } yield ()

  // Equivalent to ASL value then ORA value
  def SLO(addressMode: AddressMode): Op = for {
    address <- addressMode
    d       <- address.read()
    temp1 = d << 1
    _ <- setFlag(CpuFlags.C, temp1 & 0xff00)
    _ <- address.write(temp1 & 0x00ff)
    a <- getA
    temp2 = (a | temp1) & 0xff
    _ <- setA(temp2)
    _ <- setFlag(CpuFlags.Z, temp2 == 0x00)
    _ <- setFlag(CpuFlags.N, temp2 & 0x80)
  } yield ()

  // Equivalent to ROL value then AND value
  def RLA(addressMode: AddressMode): Op = for {
    address <- addressMode
    d       <- address.read()
    c       <- getFlag(CpuFlags.C)
    lsb  = if (c) 1 else 0
    temp = (d << 1) | lsb
    _ <- setFlag(CpuFlags.C, temp & 0xff00)
    _ <- address.write(temp & 0xff)
    a <- getA
    r = a & temp & 0xff
    _ <- setA(r)
    _ <- setFlag(CpuFlags.Z, r == 0x00)
    _ <- setFlag(CpuFlags.N, r & 0x80)
  } yield ()

  // Equivalent to LSR value then EOR value
  def SRE(addressMode: AddressMode): Op = for {
    address <- addressMode
    d       <- address.read()
    _       <- setFlag(CpuFlags.C, d & 0x01)
    temp1 = (d >> 1) & 0xff
    _ <- address.write(temp1)
    a <- getA
    temp2 = (a ^ temp1) & 0xff
    _ <- setA(temp2)
    _ <- setFlag(CpuFlags.Z, temp2 == 0x00)
    _ <- setFlag(CpuFlags.N, temp2 & 0x80)
  } yield ()

  // Equivalent to ROR value then ADC value
  def RRA(addressMode: AddressMode): Op = for {
    address <- addressMode
    d       <- address.read()
    c       <- getFlag(CpuFlags.C)
    msb   = if (c) 1 << 7 else 0
    temp1 = (d >> 1) | msb
    _ <- setFlag(CpuFlags.C, d & 0x01)
    _ <- address.write(temp1 & 0xff)
    a <- getA
    c <- getFlag(CpuFlags.C)
    lsb   = if (c) 1 else 0
    temp2 = a + temp1 + lsb
    _ <- setFlag(CpuFlags.C, temp2 & 0xff00)
    _ <- setFlag(CpuFlags.Z, (temp2 & 0x00ff) == 0x00)
    _ <- setFlag(CpuFlags.V, (~(a ^ temp1) & (a ^ temp2)) & 0x80)
    _ <- setFlag(CpuFlags.N, temp2 & 0x80)
    _ <- setA(temp2 & 0xff)
  } yield ()

  def XXX: Op = State.pure(())

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
    0xfe -> Instr("INC/ABX", INC(ABX), 7),
    // Unofficial opcodes
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
    def getNext: State[NesState, (UInt16, UInt8)] = for {
      pc <- getPc
      d  <- cpuRead(pc)
      _  <- incPc
    } yield (pc, d)

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
        else if (Set("IMM", "ZP0", "ZPX", "ZPY", "IZX", "IZY", "REL").contains(addressMode)) getNextN(1)
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