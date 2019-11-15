package scalanes

import cats.Monad
import cats.implicits._
import com.typesafe.scalalogging.LazyLogging
import scalanes.CpuFlags.CpuFlags

case class CpuState(a: UInt8, x: UInt8, y: UInt8, stkp: UInt8, pc: UInt16, status: UInt8, cycles: Int) {

  def getFlag(flag: CpuFlags): Boolean = status & flag.bit

}

object CpuState {
  val initial: CpuState = CpuState(0x00, 0x00, 0x00, 0xFD, 0x0000, 0x00 | CpuFlags.U.bit | CpuFlags.I.bit, 0)
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
    require((address & 0xFFFF) == address)
    override def read(): State[NesState, UInt8] = cpuRead(address)
    override def write(d: UInt8): State[NesState, Unit] = cpuWrite(address, d)
  }

  case object AccAddress extends Address {
    override def read(): State[NesState, UInt8] = getA
    override def write(d: UInt8): State[NesState, Unit] = setA(d)
  }

  type AddressMode = State[NesState, _ <: Address]
  type AbsAddressMode = State[NesState, AbsAddress]
  type AccAddressMode = State[NesState, AccAddress.type]
  type RelAddressMode = State[NesState, UInt16]
  type Op = State[NesState, Unit]
  type CpuOp = State[CpuState, Unit]

  implicit class CpuStateOps[A](val a: State[CpuState, A]) extends AnyVal {
    def toNesState: State[NesState, A] = a.transformS(
      NesState.cpuState.get,
      (nesState, cpuState) => NesState.cpuState.set(cpuState)(nesState)
    )
  }

  private def readExecute(addressMode: AddressMode)(op: UInt8 => CpuState => CpuState): Op = for {
    address <- addressMode
    d <- address.read()
    _ <- State.modify(NesState.cpuState.modify(op(d)))
  } yield ()

  private def readExecuteWrite(addressMode: AddressMode)(op: UInt8 => CpuState => (CpuState, UInt8)): Op = for {
    address <- addressMode
    dIn <- address.read()
    dOut <- State(op(dIn)).toNesState
    _ <- address.write(dOut)
  } yield ()

  private def executeWrite(addressMode: AddressMode)(op: CpuState => (CpuState, UInt8)): Op = for {
    address <- addressMode
    dOut <- State(op).toNesState
    _ <- address.write(dOut)
  } yield ()

  def incPc: State[NesState, UInt16] = State { s =>
    val updated = NesState.pc.modify(pc => (pc + 1) & 0xFFFF)(s)
    (updated, NesState.pc.get(updated))
  }

  def decPc: State[NesState, UInt16] = State { s =>
    val updated = NesState.pc.modify(pc => (pc - 1) & 0xFFFF)(s)
    (updated, NesState.pc.get(updated))
  }

  def getPc: State[NesState, UInt16] = State.inspect(NesState.pc.get)

  def setPc(d: UInt16): State[NesState, Unit] = {
    require((d & 0xFFFF) == d)
    State.modify(NesState.pc.set(d))
  }

  def getX: State[NesState, UInt8] = State.inspect(NesState.x.get)

  def setX(d: UInt8): State[NesState, Unit] = {
    require((d & 0xFF) == d)
    State.modify(NesState.x.set(d))
  }

  def getY: State[NesState, UInt8] = State.inspect(NesState.y.get)

  def setY(d: UInt8): State[NesState, Unit] = {
    require((d & 0xFF) == d)
    State.modify(NesState.y.set(d))
  }

  def getA: State[NesState, UInt8] = State.inspect(NesState.a.get)

  def setA(d: UInt8): State[NesState, Unit] = {
    require((d & 0xFF) == d)
    State.modify(NesState.a.set(d))
  }

  def getStkp: State[NesState, UInt8] = State.inspect(NesState.stkp.get)

  def setStkp(d: UInt8): State[NesState, Unit] = {
    require((d & 0xFF) == d)
    State.modify(NesState.stkp.set(d))
  }

  def decStkp: State[NesState, UInt8] = State { s =>
    val updated = NesState.stkp.modify(stkp => (stkp - 1) & 0xFF)(s)
    (updated, NesState.stkp.get(updated))
  }

  def incStkp: State[NesState, UInt8] = State { s =>
    val updated = NesState.stkp.modify(stkp => (stkp + 1) & 0xFF)(s)
    (updated, NesState.stkp.get(updated))
  }

  def getCycles: State[NesState, Int] = State.inspect(NesState.cycles.get)

  def setCycles(d: Int): State[NesState, Unit] = State.modify(NesState.cycles.set(d))

  def incCycles(n: Int): State[NesState, Int] = State.modify(NesState.cycles.modify(_ + 1)).get.map(NesState.cycles.get)

  def decCycles(n: Int): State[NesState, Int] = State.modify(NesState.cycles.modify(_ - 1)).get.map(NesState.cycles.get)

  def cpuRead(address: UInt16): State[NesState, UInt8] = {
    require((address & 0xFFFF) == address)
    if (address >= 0x0000 && address <= 0x1FFF)       // RAM
      State.inspect(_.ram(address % 0x800))
    else if (address >= 0x2000 && address <= 0x3FFF)  // PPU registers
      Ppu.cpuRead(address)
    else if (address == 0x4016)                       // Controller 1
      Controller.serialReadController1
    else if (address == 0x4017)                       // Controller 2
      Controller.serialReadController2
    else if (address >= 0x6000 && address <= 0xFFFF)  // Cartridge
      Cartridge.cpuRead(address)
    else
      State.pure(0x00)
  }

  def cpuWrite(address: UInt16, d: UInt8): State[NesState, Unit] = {
    require((address & 0xFFFF) == address)
    require((d & 0xFF) == d)
    if (address >= 0x0000 && address <= 0x1FFF)         //RAM
      State.modify(NesState.ram.modify(_.updated(address, d)))
    else if (address >= 0x2000 && address <= 0x3FFF)    // PPU registers
      Ppu.cpuWrite(address, d)
    else if (address == 0x4014) {                       // OAM DMA
      val page = d << 8
      incCycles(513) *> (0 until 256).map { oamAddress =>
        cpuRead(page | oamAddress).flatMap(Ppu.writeOam(oamAddress, _))
      }.reduce(_ *> _)
    } else if (address == 0x4016 && (d & 0x01))         // Controller 1
      Controller.writeController1
    else if (address == 0x4017 && (d & 0x01))           // Controller 2
      Controller.writeController2
    else if (address >= 0x6000 && address <= 0xFFFF)    // Cartridge
      Cartridge.cpuWrite(address, d)
    else
      State.pure(())
  }

  def getStatus: State[NesState, UInt8] = State.inspect(_.cpuState.status)

  def setStatus(d: UInt8): State[NesState, Unit] = {
    require((d & 0xFF) == d)
    State.modify(NesState.status.set(d))
  }

  def setFlag(flag: CpuFlags, value: Boolean): State[NesState, Unit] = State.modify(
    NesState.status.modify(s => if (value) s | flag.bit else s & ~flag.bit)
  )

  def setFlags(flags: (CpuFlags, Boolean)*)(s: UInt8): UInt8 =
    flags.foldLeft(s) { case (acc, (f, v)) => if (v) acc | f.bit else acc & ~f.bit }

  def setZnFlags(d: UInt8)(s: UInt8): UInt8 =
    setFlags((CpuFlags.Z, d == 0x00), (CpuFlags.N, d & 0x80))(s)

  def getFlag(flag: CpuFlags): State[NesState, Boolean] = State.inspect(flag.bit & _.cpuState.status)

  def pop: State[NesState, UInt8] = for {
    stkp <- incStkp
    d <- cpuRead((0x0100 + stkp) & 0xFFFF)
  } yield d

  def push(d: UInt8): State[NesState, Unit] = for {
    stkp <- getStkp
    _ <- cpuWrite((0x0100 + stkp) & 0xFFFF, d)
    _ <- decStkp
  } yield ()

  def isPageChange(a: Int, i: Int): Boolean = ((a + i) & 0xFF00) != (a & 0xFF00)

  private def asUInt16(hi: UInt8, lo: UInt8): UInt16 = {
    require((hi & 0xFF) == hi)
    require((lo & 0xFF) == lo)
    (hi << 8) | lo
  }

  def reset: State[NesState, Unit] = for {
    lo <- cpuRead(0xFFFC)
    hi <- cpuRead(0xFFFD)
    _ <- setPc(asUInt16(hi, lo))
    _ <- setA(0)
    _ <- setX(0)
    _ <- setY(0)
    _ <- setStkp(0xFD)
    _ <- setStatus(0x00 | CpuFlags.U.bit | CpuFlags.I.bit)
    _ <- setCycles(0)
  } yield ()

  def irq: State[NesState, Unit] = for {
    pc <- getPc
    pcHi = (pc >> 8) & 0xFF
    _ <- push(pcHi)
    pcLo = pc & 0xFF
    _ <- push(pcLo)
    _ <- setFlag(CpuFlags.B, value = false)
    _ <- setFlag(CpuFlags.U, value = true)
    _ <- setFlag(CpuFlags.I, value = true)
    status <- getStatus
    _ <- push(status)
    lo <- cpuRead(0xFFFE)
    hi <- cpuRead(0xFFFE+ 1)
    _ <- setPc(asUInt16(hi, lo))
    _ <- setCycles(7)
  } yield ()

  def nmi: State[NesState, NesState] = for {
    pc <- getPc
    pcHi = (pc >> 8) & 0xFF
    _ <- push(pcHi)
    pcLo = pc & 0xFF
    _ <- push(pcLo)
    _ <- setFlag(CpuFlags.B, value = false)
    _ <- setFlag(CpuFlags.U, value = true)
    _ <- setFlag(CpuFlags.I, value = true)
    status <- getStatus
    _ <- push(status)
    lo <- cpuRead(0xFFFA)
    hi <- cpuRead(0xFFFA + 1)
    _ <- setPc(asUInt16(hi, lo))
    _ <- setCycles(8)
    s <- State.get
  } yield s

  val clock: State[NesState, NesState] = State.get.flatMap { ns =>
    if (ns.cpuState.cycles == 0)
      cpuRead(ns.cpuState.pc).flatMap { opCode =>
        val instr = lookup(opCode)
        State[NesState, Unit] { ns =>
          val updated = (NesState.cycles.set(instr.cycles) andThen NesState.pc.modify(_ + 1))(ns)
          (updated, ())
        } *> instr.op *> setFlag(CpuFlags.U, value = true).get
      }
    else
      State { ns =>
        val updated = NesState.cycles.modify(_ - 1)(ns)
        (updated, updated)
      }
  }

  def executeNextInstr: State[NesState, Unit] = for {
    _ <- clock
    _ <- Monad[State[NesState, *]].whileM_(getCycles.map(_ != 0))(clock)
  } yield ()

  val IMP: AccAddressMode = State.pure(AccAddress)

  val IMM: AbsAddressMode = State { nesState =>
    val pc = nesState.cpuState.pc
    (NesState.pc.modify(_ + 1)(nesState), AbsAddress(pc))
  }

  val ZP0: AbsAddressMode = for {
    pc <- IMM
    address <- pc.read()
  } yield AbsAddress(address)

  val ZPX: AbsAddressMode = for {
    pc <- IMM
    address <- pc.read()
    x <- getX
  } yield AbsAddress((address + x) & 0xFF)

  val ZPY: AbsAddressMode = for {
    pc <- IMM
    address <- pc.read()
    y <- getY
  } yield AbsAddress((address + y) & 0xFF)

  val REL: RelAddressMode = for {
    pc <- IMM
    address <- pc.read()
    // Preserve sign
    r = address.toByte
  } yield r

  val ABS: AbsAddressMode = for {
    pc1 <- IMM
    lo <- pc1.read()
    pc2 <- IMM
    hi <- pc2.read()
    address = asUInt16(hi, lo)
  } yield AbsAddress(address)

  val ABX: AbsAddressMode = for {
    abs <- ABS
    address = abs.address
    x <- getX
    c = if (isPageChange(address, x)) 1 else 0
    _ <- incCycles(c)
  } yield AbsAddress((address + x) & 0xFFFF)

  val ABY: AbsAddressMode = for {
    abs <- ABS
    address = abs.address
    y <- getY
    c = if (isPageChange(address, y)) 1 else 0
    _ <- incCycles(c)
  } yield AbsAddress((address + y) & 0xFFFF)

  val IND: AbsAddressMode = for {
    abs <- ABS
    ptr = abs.address
    lo <- cpuRead(ptr)
    hi <- if ((ptr & 0x00FF) == 0x00FF)
      cpuRead(ptr & 0xFF00)
    else
      cpuRead((ptr + 1) & 0xFFFF)
    address = asUInt16(hi, lo)
  } yield AbsAddress(address)

  val IZX: AbsAddressMode = for {
    pc <- IMM
    t <- pc.read()
    x <- getX
    lo <- cpuRead((t + x + 0) & 0x00FF)
    hi <- cpuRead((t + x + 1) & 0x00FF)
    address = asUInt16(hi, lo)
  } yield AbsAddress(address)

  val IZY: AbsAddressMode = for {
    pc <- IMM
    t <- pc.read()
    y <- getY
    lo <- cpuRead((t + 0) & 0x00FF)
    hi <- cpuRead((t + 1) & 0x00FF)
    address = asUInt16(hi, lo)
    c = if (isPageChange(address, y)) 1 else 0
    _ <- incCycles(c)
  } yield AbsAddress((address + y) & 0xFFFF)

  def ADC(addressMode: AddressMode): Op = readExecute(addressMode) { d => cpuState =>
    val c = cpuState.getFlag(CpuFlags.C)
    val lsb = if (c) 1 else 0
    val temp = cpuState.a + d + lsb
    val status = setFlags(
      (CpuFlags.C, temp & 0xFF00),
      (CpuFlags.Z, (temp & 0x00FF) == 0x00),
      (CpuFlags.V, (~(cpuState.a ^ d) & (cpuState.a ^ temp)) & 0x80),
      (CpuFlags.N, temp & 0x80)
    )(cpuState.status)
    cpuState.copy(a = temp & 0xFF, status = status)
  }

  def AND(addressMode: AddressMode): Op = readExecute(addressMode) { d => cpuState =>
    val r = cpuState.a & d & 0xFF
    val status = setZnFlags(r)(cpuState.status)
    cpuState.copy(a = r, status = status)
  }

  def ASL(addressMode: AddressMode): Op = readExecuteWrite(addressMode) { d => cpuState =>
    val temp = d << 1
    val status = setFlags(
      (CpuFlags.C, temp & 0xFF00),
      (CpuFlags.Z, (temp & 0x00FF) == 0x00),
      (CpuFlags.N, temp & 0x80)
    )(cpuState.status)
    (cpuState.copy(status = status), temp & 0xFF)
  }

  def branch: Op = REL.transform { (nesState, relAddress) =>
    val cpuState = nesState.cpuState
    val absAddress = (cpuState.pc + relAddress) & 0xFFFF
    val cycles = cpuState.cycles + (if (isPageChange(cpuState.pc, relAddress)) 2 else 1)
    val updated = cpuState.copy(pc = absAddress, cycles = cycles)
    (nesState.copy(cpuState = updated), ())
  }

  def continue: Op = incPc.map(_ => Unit)

  val BCC: Op = State.get.flatMap { ns =>
    if (ns.cpuState.getFlag(CpuFlags.C))
      continue
    else
      branch
  }

  val BCS: Op = State.get.flatMap { ns =>
    if (ns.cpuState.getFlag(CpuFlags.C))
      branch
    else
      continue
  }

  val BEQ: Op = State.get.flatMap { ns =>
    if (ns.cpuState.getFlag(CpuFlags.Z))
      branch
    else
      continue
  }

  def BIT(addressMode: AddressMode): Op = readExecute(addressMode) { d => cpuState =>
    val temp = cpuState.a & d
    val status = setFlags(
      (CpuFlags.Z, (temp & 0x00FF) == 0x00),
      (CpuFlags.N, d & (1 << 7)),
      (CpuFlags.V, d & (1 << 6))
    )(cpuState.status)
    cpuState.copy(status = status)
  }

  val BMI: Op = State.get.flatMap { ns =>
    if (ns.cpuState.getFlag(CpuFlags.N))
      branch
    else
      continue
  }

  val BNE: Op = State.get.flatMap { ns =>
    if (ns.cpuState.getFlag(CpuFlags.Z))
      continue
    else
      branch
  }

  val BPL: Op = State.get.flatMap { ns =>
    if (ns.cpuState.getFlag(CpuFlags.N))
      continue
    else
      branch
  }

  def BRK: Op = for {
    pc1 <- incPc
    _ <- setFlag(CpuFlags.I, value = true)
    _ <- push((pc1 >> 8) & 0xFF)
    _ <- push(pc1 & 0xFF)
    _ <- setFlag(CpuFlags.B, value = true)
    status <- getStatus
    _ <- push(status)
    _ <- setFlag(CpuFlags.B, value = false)
    d1 <- cpuRead(0xFFFE)
    d2 <- cpuRead(0xFFFF)
    pc2 = asUInt16(d2, d1)
    _ <- setPc(pc2)
  } yield ()

  val BVC: Op = State.get.flatMap { ns =>
    if (ns.cpuState.getFlag(CpuFlags.V))
      continue
    else
      branch
  }

  val BVS: Op = State.get.flatMap { ns =>
    if (ns.cpuState.getFlag(CpuFlags.V))
      branch
    else
      continue
  }

  def CLC: Op = setFlag(CpuFlags.C, value = false)

  def CLD: Op = setFlag(CpuFlags.D, value = false)

  def CLI: Op = setFlag(CpuFlags.I, value = false)

  def CLV: Op = setFlag(CpuFlags.V, value = false)

  def compareS(getter1: State[NesState, UInt8], getter2: State[NesState, UInt8]): Op = for {
    d1 <- getter1
    d2 <- getter2
    temp = d2 - d1
    _ <- setFlag(CpuFlags.C, d2 >= d1)
    _ <- setFlag(CpuFlags.Z, (temp & 0x00FF) == 0x0000)
    _ <- setFlag(CpuFlags.N, temp & 0x0080)
  } yield ()

  def compare(getter: CpuState => UInt8): UInt8 => CpuState => CpuState = { d1 => cpuState =>
    val d2 = getter(cpuState)
    val temp = d2 - d1
    val status = setFlags(
      (CpuFlags.C, d2 >= d1),
      (CpuFlags.Z, (temp & 0x00FF) == 0x0000),
      (CpuFlags.N, temp & 0x0080)
    )(cpuState.status)
    cpuState.copy(status = status)
  }

  def CMP(addressMode: AddressMode): Op = readExecute(addressMode)(compare(_.a))

  def CPX(addressMode: AddressMode): Op = readExecute(addressMode)(compare(_.x))

  def CPY(addressMode: AddressMode): Op = readExecute(addressMode)(compare(_.y))

  def DEC(addressMode: AddressMode): Op = readExecuteWrite(addressMode) { d => cpuState =>
    val temp = (d - 1) & 0xFF
    val status = setZnFlags(temp)(cpuState.status)
    (cpuState.copy(status = status), temp)
  }

  def DEX: Op = State.modify { nesState =>
    val cpuState = nesState.cpuState
    val temp = (cpuState.x - 1) & 0xFF
    val status = setZnFlags(temp)(cpuState.status)
    val updated = cpuState.copy(status = status, x = temp)
    nesState.copy(cpuState = updated)
  }

  def DEY: Op = State.modify { nesState =>
    val cpuState = nesState.cpuState
    val temp = (cpuState.y - 1) & 0xFF
    val status = setZnFlags(temp)(cpuState.status)
    val updated = cpuState.copy(status = status, y = temp)
    nesState.copy(cpuState = updated)
  }

  def EOR(addressMode: AddressMode): Op = readExecute(addressMode) { d => cpuState =>
    val temp = (cpuState.a ^ d) & 0xFF
    val status = setZnFlags(temp)(cpuState.status)
    cpuState.copy(a = temp, status = status)
  }

  def INC(addressMode: AddressMode): Op = readExecuteWrite(addressMode) { d => cpuState =>
    val temp = (d + 1) & 0xFF
    val status = setZnFlags(temp)(cpuState.status)
    (cpuState.copy(status = status), temp)
  }

  def INX: Op = State.modify { nesState =>
    val cpuState = nesState.cpuState
    val temp = (cpuState.x + 1) & 0xFF
    val status = setZnFlags(temp)(cpuState.status)
    val updated = cpuState.copy(status = status, x = temp)
    nesState.copy(cpuState = updated)
  }

  def INY: Op = State.modify { nesState =>
    val cpuState = nesState.cpuState
    val temp = (cpuState.y + 1) & 0xFF
    val status = setZnFlags(temp)(cpuState.status)
    val updated = cpuState.copy(status = status, y = temp)
    nesState.copy(cpuState = updated)
  }

  def JMP(addressMode: AbsAddressMode): Op = for {
    address <- addressMode
    _ <- setPc(address.address)
  } yield ()

  def JSR(addressMode: AbsAddressMode): Op = for {
    address <- addressMode
    pc <- decPc
    _ <- push((pc >> 8) & 0xFF)
    _ <- push(pc & 0xFF)
    _ <- setPc(address.address)
  } yield ()

  def LDA(addressMode: AddressMode): Op = readExecute(addressMode) { d => cpuState =>
    val status = setZnFlags(d)(cpuState.status)
    cpuState.copy(a = d, status = status)
  }

  def LDX(addressMode: AddressMode): Op = readExecute(addressMode) { d => cpuState =>
    val status = setZnFlags(d)(cpuState.status)
    cpuState.copy(x = d, status = status)
  }

  def LDY(addressMode: AddressMode): Op = readExecute(addressMode) { d => cpuState =>
    val status = setZnFlags(d)(cpuState.status)
    cpuState.copy(y = d, status = status)
  }

  def LSR(addressMode: AddressMode): Op = readExecuteWrite(addressMode) { d => cpuState =>
    val temp = (d >> 1) & 0xFF
    val status = setFlags(
      (CpuFlags.C, d & 0x01),
      (CpuFlags.Z, (temp & 0xFF) == 0x00),
      (CpuFlags.N, temp & 0x80)
    )(cpuState.status)
    (cpuState.copy(status = status), temp)
  }

  def NOP(addressMode: AddressMode): Op = for {
    address <- addressMode
    _ <- address.read()
  } yield ()

  def ORA(addressMode: AddressMode): Op = readExecute(addressMode) { d => cpuState =>
    val temp = (cpuState.a | d) & 0xFF
    val status = setZnFlags(temp)(cpuState.status)
    cpuState.copy(a = temp, status = status)
  }

  def PHA: Op = for {
    a <- getA
    _ <- push(a)
  } yield ()

  def PHP: Op = for {
    status <- getStatus
    _ <- push(status | CpuFlags.B.bit | CpuFlags.U.bit)
    _ <- setFlag(CpuFlags.B, value = false)
    _ <- setFlag(CpuFlags.U, value = false)
  } yield ()

  def PLA: Op = for {
    d <- pop
    _ <- setA(d)
    _ <- setFlag(CpuFlags.Z, d == 0x00)
    _ <- setFlag(CpuFlags.N, d & 0x80)
  } yield ()

  def PLP: Op = for {
    status <- pop
    _ <- setStatus(status)
    _ <- setFlag(CpuFlags.U, value = true)
  } yield ()

  def ROL(addressMode: AddressMode): Op = readExecuteWrite(addressMode) { d => cpuState =>
    val c = cpuState.getFlag(CpuFlags.C)
    val lsb = if (c) 1 else 0
    val temp = (d << 1) | lsb
    val status = setFlags(
      (CpuFlags.C, temp & 0xFF00),
      (CpuFlags.Z, (temp & 0x00FF) == 0x00),
      (CpuFlags.N, temp & 0x80)
    )(cpuState.status)
    (cpuState.copy(status = status), temp & 0xFF)
  }

  def ROR(addressMode: AddressMode): Op = readExecuteWrite(addressMode) { d => cpuState =>
    val c = cpuState.getFlag(CpuFlags.C)
    val msb = if (c) 1 << 7 else 0
    val temp = (d >> 1) | msb
    val status = setFlags(
      (CpuFlags.C, d & 0x01),
      (CpuFlags.Z, (temp & 0x00FF) == 0x00),
      (CpuFlags.N, temp & 0x80)
    )(cpuState.status)
    (cpuState.copy(status = status), temp & 0xFF)
  }

  def RTI: Op = for {
    status1 <- pop
    status2 = status1 & ~CpuFlags.B.bit & ~CpuFlags.U.bit
    _ <- setStatus(status2)
    pc1 <- pop
    pc2 <- pop
    pc = asUInt16(pc2, pc1)
    _ <- setPc(pc)
  } yield ()

  def RTS: Op = for {
    pc1 <- pop
    pc2 <- pop
    pc = asUInt16(pc2, pc1)
    _ <- setPc(pc)
    _ <- incPc
  } yield ()

  def SBC(addressMode: AddressMode): Op = readExecute(addressMode) { d => cpuState =>
    val value = d ^ 0x00FF
    val c = cpuState.getFlag(CpuFlags.C)
    val lsb = if (c) 1 else 0
    val temp = cpuState.a + value + lsb
    val status = setFlags(
      (CpuFlags.C, temp & 0xFF00),
      (CpuFlags.Z, (temp & 0x00FF) == 0x00),
      (CpuFlags.V, (temp ^ cpuState.a) & (temp ^ value) & 0x80),
      (CpuFlags.N, temp & 0x80)
    )(cpuState.status)
    cpuState.copy(a = temp & 0xFF, status = status)
  }

  def SEC: Op = setFlag(CpuFlags.C, value = true)

  def SED: Op = setFlag(CpuFlags.D, value = true)

  def SEI: Op = setFlag(CpuFlags.I, value = true)

  def STA(addressMode: AddressMode): Op = executeWrite(addressMode) { cpuState =>
    (cpuState, cpuState.a)
  }

  def STX(addressMode: AddressMode): Op = executeWrite(addressMode) { cpuState =>
    (cpuState, cpuState.x)
  }

  def STY(addressMode: AddressMode): Op = executeWrite(addressMode) { cpuState =>
    (cpuState, cpuState.y)
  }

  def TAX: Op = State.modify { nesState =>
    val cpuState = nesState.cpuState
    val status = setZnFlags(cpuState.a)(cpuState.status)
    val updated = cpuState.copy(x = cpuState.a, status = status)
    nesState.copy(cpuState = updated)
  }

  def TAY: Op = State.modify { nesState =>
    val cpuState = nesState.cpuState
    val status = setZnFlags(cpuState.a)(cpuState.status)
    val updated = cpuState.copy(y = cpuState.a, status = status)
    nesState.copy(cpuState = updated)
  }

  def TSX: Op = State.modify { nesState =>
    val cpuState = nesState.cpuState
    val status = setZnFlags(cpuState.stkp)(cpuState.status)
    val updated = cpuState.copy(x = cpuState.stkp, status = status)
    nesState.copy(cpuState = updated)
  }

  def TXA: Op = State.modify { nesState =>
    val cpuState = nesState.cpuState
    val status = setZnFlags(cpuState.x)(cpuState.status)
    val updated = cpuState.copy(a = cpuState.x, status = status)
    nesState.copy(cpuState = updated)
  }

  def TXS: Op = State.modify { nesState =>
    val cpuState = nesState.cpuState
    val updated = cpuState.copy(stkp = cpuState.x)
    nesState.copy(cpuState = updated)
  }

  def TYA: Op = State.modify { nesState =>
    val cpuState = nesState.cpuState
    val status = setZnFlags(cpuState.y)(cpuState.status)
    val updated = cpuState.copy(a = cpuState.y, status = status)
    nesState.copy(cpuState = updated)
  }

  // Unofficial instructions

  def LAX(addressMode: AddressMode): Op = for {
    _ <- LDA(addressMode)
    _ <- TAX
  } yield ()

  def SAX(addressMode: AddressMode): Op = for {
    address <- addressMode
    a <- getA
    x <- getX
    d = a & x
    _ <- address.write(d)
  } yield ()

  def DCP(addressMode: AddressMode): Op = for {
    address <- addressMode
    d <- address.read()
    temp = (d - 1) & 0xFF
    _ <- address.write(temp)
    _ <- setFlag(CpuFlags.Z, temp == 0x00)
    _ <- setFlag(CpuFlags.N, temp & 0x80)
    _ <- compareS(address.read(), getA)
  } yield ()

  def ISC(addressMode: AddressMode): Op = for {
    address <- addressMode
    d <- address.read()
    temp1 = (d + 1) & 0xFF
    _ <- address.write(temp1)
    value = temp1 ^ 0x00FF
    a <- getA
    c <- getFlag(CpuFlags.C)
    lsb = if (c) 1 else 0
    temp2 = a + value + lsb
    _ <- setFlag(CpuFlags.C, temp2 & 0xFF00)
    _ <- setFlag(CpuFlags.Z, (temp2 & 0x00FF) == 0x00)
    _ <- setFlag(CpuFlags.V, (temp2 ^ a) & (temp2 ^ value) & 0x80)
    _ <- setFlag(CpuFlags.N, temp2 & 0x80)
    _ <- setA(temp2 & 0xFF)
  } yield ()

  def SLO(addressMode: AddressMode): Op = for {
    address <- addressMode
    d <- address.read()
    temp1 = d << 1
    _ <- setFlag(CpuFlags.C, temp1 & 0xFF00)
    _ <- address.write(temp1 & 0x00FF)
    a <- getA
    temp2 = (a | temp1) & 0xFF
    _ <- setA(temp2)
    _ <- setFlag(CpuFlags.Z, temp2 == 0x00)
    _ <- setFlag(CpuFlags.N, temp2 & 0x80)
  } yield ()

  def RLA(addressMode: AddressMode): Op = for {
    address <- addressMode
    d <- address.read()
    c <- getFlag(CpuFlags.C)
    lsb = if (c) 1 else 0
    temp = (d << 1) | lsb
    _ <- setFlag(CpuFlags.C, temp & 0xFF00)
    _ <- address.write(temp & 0xFF)
    a <- getA
    r = a & temp & 0xFF
    _ <- setA(r)
    _ <- setFlag(CpuFlags.Z, r == 0x00)
    _ <- setFlag(CpuFlags.N, r & 0x80)
  } yield ()

  def SRE(addressMode: AddressMode): Op = for {
    address <- addressMode
    d <- address.read()
    _ <- setFlag(CpuFlags.C, d & 0x01)
    temp1 = (d >> 1) & 0xFF
    _ <- address.write(temp1)
    a <- getA
    temp2 = (a ^ temp1) & 0xFF
    _ <- setA(temp2)
    _ <- setFlag(CpuFlags.Z, temp2 == 0x00)
    _ <- setFlag(CpuFlags.N, temp2 & 0x80)
  } yield ()

  def RRA(addressMode: AddressMode): Op = for {
    address <- addressMode
    d <- address.read()
    c <- getFlag(CpuFlags.C)
    msb = if (c) 1 << 7 else 0
    temp1 = (d >> 1) | msb
    _ <- setFlag(CpuFlags.C, d & 0x01)
    _ <- address.write(temp1 & 0xFF)
    a <- getA
    c <- getFlag(CpuFlags.C)
    lsb = if (c) 1 else 0
    temp2 = a + temp1 + lsb
    _ <- setFlag(CpuFlags.C, temp2 & 0xFF00)
    _ <- setFlag(CpuFlags.Z, (temp2 & 0x00FF) == 0x00)
    _ <- setFlag(CpuFlags.V, (~(a ^ temp1) & (a ^ temp2)) & 0x80)
    _ <- setFlag(CpuFlags.N, temp2 & 0x80)
    _ <- setA(temp2 & 0xFF)
  } yield ()

  def XXX: Op = State.pure(())

  case class Instr(info: String, op: Op, cycles: Int)

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
  ).withDefault { d =>
    if (Set(0x80, 0x82, 0xC2, 0xE2, 0x89).contains(d))
      Instr("NOP/IMM", NOP(IMM), 2)
    else if (Set(0x1A, 0x3A, 0x5A, 0x7A, 0xDA, 0xFA).contains(d))
      Instr("NOP/IMP", NOP(IMP), 2)
    else if (Set(0x0C).contains(d))
      Instr("NOP/ABS", NOP(ABS), 4)
    else if (Set(0x14, 0x34, 0x54, 0x74, 0xD4, 0xF4).contains(d))
      Instr("NOP/ZPX", NOP(ZPX), 4)
    else if (Set(0x1C, 0x3C, 0x5C, 0x7C, 0xDC, 0xFC).contains(d))
      Instr("NOP/ABX", NOP(ABX), 4)
    else if (Set(0x04, 0x44, 0x64).contains(d))
      Instr("NOP/ZP0", NOP(ZP0), 3)
    else if (Set(0x02, 0x12, 0x22, 0x32, 0x42, 0x52, 0x62, 0x72, 0x92, 0xB2, 0xD2, 0xF2, 0x0B, 0x2B, 0x4B, 0x6B, 0x8B, 0xCB).contains(d))
      Instr("XXX/IMP", XXX, 2)
    else if (Set(0xBB).contains(d))
      Instr("XXX/IMP", XXX, 4)
    else if (Set(0x9B, 0x9E, 0x9F).contains(d))
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
      d <- cpuRead(pc)
      _ <- incPc
    } yield (pc, d)

    def getNextN(n: Int): State[NesState, List[UInt8]] =
      Monad[State[NesState, *]].replicateA(n, getNext).map(_.map(_._2))

    for {
      next <- getNext
      (addr, d) = next
      infoParts = lookup(d).info.split('/')
      opcode = infoParts.head
      addressMode = infoParts.last
      cmdParts <- if (addressMode == "IMP") getNextN(0)
        else if (Set("IMM", "ZP0", "ZPX", "ZPY", "IZX", "IZY", "REL").contains(addressMode)) getNextN(1)
        else if (Set("ABS", "ABX", "ABY", "IND").contains(addressMode)) getNextN(2)
        else throw new RuntimeException(s"Unexpected address mode: $addressMode")
      res = if (addressMode == "IMP")
          s"$opcode {IMP}"
        else if (addressMode == "IMM")
          s"$opcode #$$${hex(cmdParts.head, 2)} {IMM}"
        else if (addressMode == "ZP0")
          s"$opcode $$${hex(cmdParts.head, 2)} {ZP0}"
        else if (addressMode == "ZPX")
          s"$opcode $$${hex(cmdParts.head, 2)}, X {ZPX}"
        else if (addressMode == "ZPY")
          s"$opcode $$${hex(cmdParts.head, 2)}, Y {ZPY}"
        else if (addressMode == "IZX")
          s"$opcode ($$${hex(cmdParts.head, 2)}, X) {IZX}"
        else if (addressMode == "IZY")
          s"$opcode ($$${hex(cmdParts.head, 2)}, Y) {IZY}"
        else if (addressMode == "ABS") {
          val a = asUInt16(cmdParts(1), cmdParts.head)
          s"$opcode $$${hex(a, 4)} {ABS}"
        } else if (addressMode == "ABX") {
          val a = asUInt16(cmdParts(1), cmdParts.head)
          s"$opcode $$${hex(a, 4)}, X {ABX}"
        } else if (addressMode == "ABY") {
          val a = asUInt16(cmdParts(1), cmdParts.head)
          s"$opcode $$${hex(a, 4)}, Y {ABY}"
        } else if (addressMode == "IND") {
          val a = asUInt16(cmdParts(1), cmdParts.head)
          s"$opcode ($$${hex(a, 4)}) {IND}"
        } else if (addressMode == "REL") {
          val a = cmdParts.head
          s"$opcode $$${hex(a, 2)} [$$${hex(addr + 2 + a.toByte, 4)}] {REL}"
        } else
          throw new RuntimeException(s"Unexpected address mode: $addressMode")
    } yield addr -> res
  }

  def disassemble(n: Int): State[NesState, List[(UInt16, String)]] =
    Monad[State[NesState, *]].replicateA(n, disassemble)

}
