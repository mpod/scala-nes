package scalanes

import cats.Monad
import cats.data.State
import com.typesafe.scalalogging.LazyLogging
import scalanes.CpuFlags.CpuFlags

import scala.collection.immutable.Queue

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

  def incCycles(n: Int): State[NesState, Int] = State { s =>
    val updated = NesState.cycles.modify(cycles => cycles + 1)(s)
    (updated, NesState.cycles.get(updated))
  }

  def decCycles(n: Int): State[NesState, Int] = State { s =>
    val updated = NesState.cycles.modify(cycles => cycles - 1)(s)
    (updated, NesState.cycles.get(updated))
  }

  def cpuRead(address: UInt16): State[NesState, UInt8] = {
    require((address & 0xFFFF) == address)
    State.inspect(_.ram(address))
  }

  def cpuWrite(address: UInt16, d: UInt8): State[NesState, Unit] = {
    require((address & 0xFFFF) == address)
    require((d & 0xFF) == d)
    State.modify(NesState.ram.modify(_.updated(address, d)))
  }

  def getStatus: State[NesState, UInt8] = State.inspect(_.cpuRegisters.status)

  def setStatus(d: UInt8): State[NesState, Unit] = {
    require((d & 0xFF) == d)
    State.modify(NesState.status.set(d))
  }

  def setFlag(flag: CpuFlags, value: Boolean): State[NesState, Unit] = State.modify(
    NesState.status.modify(s => if (value) s | flag.bit else s & ~flag.bit)
  )

  def getFlag(flag: CpuFlags): State[NesState, Boolean] = State.inspect(flag.bit & _.cpuRegisters.status)

  def pop: State[NesState, UInt8] = for {
    stkp <- incStkp
    d <- cpuRead((0x0100 + stkp) & 0xFFFF)
  } yield d

  def push(d: UInt8): State[NesState, Unit] = for {
    stkp <- getStkp
    _ <- cpuWrite((0x0100 + stkp) & 0xFFFF, d)
    _ <- decStkp
  } yield Unit

  def isPageChange(a: Int, i: Int): Boolean = ((a + i) & 0xFF00) != (a & 0xFF00)

  private def asUInt16(hi: UInt8, lo: UInt8): UInt16 = {
    require((hi & 0xFF) == hi)
    require((lo & 0xFF) == lo)
    (hi << 8) | lo
  }

  def reset: State[NesState, Unit] = for {
    lo <- cpuRead(0xFFFC)
    hi <- cpuRead(0xFFFC + 1)
    _ <- setPc(asUInt16(hi, lo))
    _ <- setA(0)
    _ <- setX(0)
    _ <- setY(0)
    _ <- setStkp(0xFD)
    _ <- setStatus(0x00 | CpuFlags.U.bit)
    _ <- setCycles(0)
  } yield Unit

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
  } yield Unit

  def nmi: State[NesState, Unit] = for {
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
  } yield Unit

  def clock: State[NesState, Unit] = Monad[State[NesState, *]]
    .ifM(getCycles.map(_ == 0))(
      ifTrue = for {
          pc <- getPc
          opCode <- cpuRead(pc)
          _ <- incPc
          instr = lookup(opCode)
          _ <- setCycles(instr.cycles)
          _ <- instr.op
          _ <- setFlag(CpuFlags.U, value = true)
        } yield Unit,
      ifFalse = State.pure(Unit)
    )
    .flatMap(_ => decCycles(1))
    .map(_ => Unit)

  def executeNextInstr: State[NesState, Unit] = for {
    _ <- clock
    _ <- Monad[State[NesState, *]].whileM_(getCycles.map(_ != 0))(clock)
  } yield Unit

  val IMP: AccAddressMode = State.pure(AccAddress)

  val IMM: AbsAddressMode = for {
    pc <- getPc
    _ <- incPc
  } yield AbsAddress(pc)

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
    address <- abs.read()
    x <- getX
    c = if (isPageChange(address, x)) 1 else 0
    _ <- incCycles(c)
  } yield AbsAddress((address + x) & 0xFFFF)

  val ABY: AbsAddressMode = for {
    abs <- ABS
    address <- abs.read()
    y <- getY
    c = if (isPageChange(address, y)) 1 else 0
    _ <- incCycles(c)
  } yield AbsAddress((address + y) & 0xFFFFFF)

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
  } yield AbsAddress(address + y)

  def ADC(addressMode: AddressMode): Op = for {
    address <- addressMode
    d <- address.read()
    a <- getA
    c <- getFlag(CpuFlags.C)
    lsb = if (c) 1 else 0
    temp = a + d + lsb
    _ <- setFlag(CpuFlags.C, temp & 0xFF00)
    _ <- setFlag(CpuFlags.Z, (temp & 0x00FF) == 0x0000)
    _ <- setFlag(CpuFlags.V, (~(a ^ d) & (a ^ temp)) & 0x0080)
    _ <- setFlag(CpuFlags.N, temp & 0x00800)
    _ <- setA(temp & 0xFF)
  } yield Unit

  def AND(addressMode: AddressMode): Op = for {
    address <- addressMode
    d <- address.read()
    a <- getA
    r = a & d & 0xFF
    _ <- setA(r)
    _ <- setFlag(CpuFlags.Z, r == 0x00)
    _ <- setFlag(CpuFlags.N, r & 0x80)
  } yield Unit

  def ASL(addressMode: AddressMode): Op = for {
    address <- addressMode
    d <- address.read()
    temp = d << 1
    _ <- setFlag(CpuFlags.C, temp & 0xFF00)
    _ <- setFlag(CpuFlags.Z, (temp & 0x00FF) == 0x00)
    _ <- setFlag(CpuFlags.N, temp & 0x80)
    _ <- address.write(temp & 0x00FF)
  } yield Unit

  def branch: Op = for {
    relAddress <- REL
    _ <- incCycles(1)
    pc <- getPc
    absAddress = (pc + relAddress) & 0xFFFF
    _ <- incCycles(if (isPageChange(pc, relAddress)) 1 else 0)
    _ <- setPc(absAddress)
  } yield Unit

  def BCC: Op = Monad[State[NesState, *]].ifM(getFlag(CpuFlags.Z))(
    ifTrue = NOP(),
    ifFalse = branch
  )

  def BCS: Op = Monad[State[NesState, *]].ifM(getFlag(CpuFlags.C))(
    ifTrue = branch,
    ifFalse = NOP()
  )

  def BEQ: Op = Monad[State[NesState, *]].ifM(getFlag(CpuFlags.Z))(
    ifTrue = branch,
    ifFalse = NOP()
  )

  def BIT(addressMode: AddressMode): Op = for {
    address <- addressMode
    d <- address.read()
    a <- getA
    temp = a & d
    _ <- setFlag(CpuFlags.Z, (temp & 0x00FF) == 0x00)
    _ <- setFlag(CpuFlags.N, d & (1 << 7))
    _ <- setFlag(CpuFlags.V, d & (1 << 6))
  } yield Unit

  def BMI: Op = Monad[State[NesState, *]].ifM(getFlag(CpuFlags.N))(
    ifTrue = branch,
    ifFalse = NOP()
  )

  def BNE: Op = Monad[State[NesState, *]].ifM(getFlag(CpuFlags.Z))(
    ifTrue = NOP(),
    ifFalse = branch
  )

  def BPL: Op = Monad[State[NesState, *]].ifM(getFlag(CpuFlags.N))(
    ifTrue = NOP(),
    ifFalse = branch
  )

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
  } yield Unit

  def BVC: Op = Monad[State[NesState, *]].ifM(getFlag(CpuFlags.V))(
    ifTrue = NOP(),
    ifFalse = branch
  )

  def BVS: Op = Monad[State[NesState, *]].ifM(getFlag(CpuFlags.V))(
    ifTrue = branch,
    ifFalse = NOP()
  )

  def CLC: Op = setFlag(CpuFlags.C, value = false)

  def CLD: Op = setFlag(CpuFlags.D, value = false)

  def CLI: Op = setFlag(CpuFlags.I, value = false)

  def CLV: Op = setFlag(CpuFlags.V, value = false)

  def compare(getter1: State[NesState, UInt8], getter2: State[NesState, UInt8]): Op = for {
    d1 <- getter1
    d2 <- getter2
    temp = d2 - d1
    _ <- setFlag(CpuFlags.C, d2 >= d1)
    _ <- setFlag(CpuFlags.Z, (temp & 0x00FF) == 0x0000)
    _ <- setFlag(CpuFlags.N, temp & 0x0080)
  } yield Unit

  def CMP(addressMode: AddressMode): Op = compare(addressMode.flatMap(_.read()), getA)

  def CPX(addressMode: AddressMode): Op = compare(addressMode.flatMap(_.read()), getX)

  def CPY(addressMode: AddressMode): Op = compare(addressMode.flatMap(_.read()), getY)

  def DEC(addressMode: AddressMode): Op = for {
    address <- addressMode
    d <- address.read()
    temp = (d - 1) & 0xFF
    _ <- address.write(temp)
    _ <- setFlag(CpuFlags.Z, temp == 0x00)
    _ <- setFlag(CpuFlags.N, temp & 0x80)
  } yield Unit

  def DEX: Op = for {
    x <- getX
    temp = (x - 1) & 0xFF
    _ <- setX(temp)
    _ <- setFlag(CpuFlags.Z, temp == 0x00)
    _ <- setFlag(CpuFlags.N, temp & 0x800)
  } yield Unit

  def DEY: Op = for {
    y <- getY
    temp = (y - 1) & 0xFF
    _ <- setY(temp)
    _ <- setFlag(CpuFlags.Z, temp == 0x00)
    _ <- setFlag(CpuFlags.N, temp & 0x800)
  } yield Unit

  def EOR(addressMode: AddressMode): Op = for {
    address <- addressMode
    d <- address.read()
    a <- getA
    temp = (a ^ d) & 0xFF
    _ <- setA(temp)
    _ <- setFlag(CpuFlags.Z, temp == 0x00)
    _ <- setFlag(CpuFlags.N, temp & 0x80)
  } yield Unit

  def INC(addressMode: AddressMode): Op = for {
    address <- addressMode
    d <- address.read()
    temp = (d + 1) & 0xFF
    _ <- address.write(temp)
    _ <- setFlag(CpuFlags.Z, temp == 0x000)
    _ <- setFlag(CpuFlags.N, temp & 0x80)
  } yield Unit

  def INX: Op = for {
    x <- getX
    temp = (x + 1) & 0xFF
    _ <- setX(temp)
    _ <- setFlag(CpuFlags.Z, temp == 0x000)
    _ <- setFlag(CpuFlags.N, temp & 0x80)
  } yield Unit

  def INY: Op = for {
    y <- getY
    temp = (y + 1) & 0xFF
    _ <- setY(temp)
    _ <- setFlag(CpuFlags.Z, temp == 0x00)
    _ <- setFlag(CpuFlags.N, temp & 0x80)
  } yield Unit

  def JMP(addressMode: AbsAddressMode): Op = for {
    address <- addressMode
    _ <- setPc(address.address)
  } yield Unit

  def JSR(addressMode: AbsAddressMode): Op = for {
    pc <- decPc
    _ <- push((pc >> 8) & 0xFF)
    _ <- push(pc & 0xFF)
    address <- addressMode
    _ <- setPc(address.address)
  } yield Unit

  def LDA(addressMode: AddressMode): Op = for {
    address <- addressMode
    d <- address.read()
    _ <- setA(d)
    _ <- setFlag(CpuFlags.Z, d == 0x00)
    _ <- setFlag(CpuFlags.N, d & 0x80)
  } yield Unit

  def LDX(addressMode: AddressMode): Op = for {
    address <- addressMode
    d <- address.read()
    _ <- setX(d)
    _ <- setFlag(CpuFlags.Z, d == 0x00)
    _ <- setFlag(CpuFlags.N, d & 0x80)
  } yield Unit

  def LDY(addressMode: AddressMode): Op = for {
    address <- addressMode
    d <- address.read()
    _ <- setY(d)
    _ <- setFlag(CpuFlags.Z, d == 0x00)
    _ <- setFlag(CpuFlags.N, d & 0x80)
  } yield Unit

  def LSR(addressMode: AddressMode): Op = for {
    address <- addressMode
    d <- address.read()
    _ <- setFlag(CpuFlags.C, d & 0x01)
    temp = (d >> 1) & 0xFF
    _ <- setFlag(CpuFlags.Z, (temp & 0xFF) == 0x00)
    _ <- setFlag(CpuFlags.N, temp & 0x80)
    _ <- address.write(temp)
  } yield Unit

  def NOP(extraCycle: Boolean = false): Op =
    if (extraCycle) incCycles(1).map(_ => Unit)
    else State.pure(Unit)

  def ORA(addressMode: AddressMode): Op = for {
    address <- addressMode
    d <- address.read()
    a <- getA
    temp = (a | d) & 0xFF
    _ <- setA(temp)
    _ <- setFlag(CpuFlags.Z, temp == 0x00)
    _ <- setFlag(CpuFlags.N, a & 0x80)
  } yield Unit

  def PHA: Op = for {
    a <- getA
    _ <- push(a)
  } yield Unit

  def PHP: Op = for {
    status <- getStatus
    _ <- push(status | CpuFlags.B.bit | CpuFlags.U.bit)
    _ <- setFlag(CpuFlags.B, value = false)
    _ <- setFlag(CpuFlags.U, value = false)
  } yield Unit

  def PLA: Op = for {
    d <- pop
    _ <- setA(d)
    _ <- setFlag(CpuFlags.Z, d == 0x00)
    _ <- setFlag(CpuFlags.N, d & 0x80)
  } yield Unit

  def PLP: Op = for {
    status <- pop
    _ <- setStatus(status)
    _ <- setFlag(CpuFlags.U, value = true)
  } yield Unit

  def ROL(addressMode: AddressMode): Op = for {
    address <- addressMode
    d <- address.read()
    c <- getFlag(CpuFlags.C)
    lsb = if (c) 1 else 0
    temp = (d << 1) | lsb
    _ <- setFlag(CpuFlags.C, temp & 0xFF00)
    _ <- setFlag(CpuFlags.Z, (temp & 0x00FF) == 0x0000)
    _ <- setFlag(CpuFlags.N, temp & 0x0080)
    _ <- address.write(temp & 0xFF)
  } yield Unit

  def ROR(addressMode: AddressMode): Op = for {
    address <- addressMode
    d <- address.read()
    c <- getFlag(CpuFlags.C)
    msb = if (c) 1 << 7 else 0
    temp = (d >> 1) | msb
    _ <- setFlag(CpuFlags.C, temp & 0x01)
    _ <- setFlag(CpuFlags.Z, (temp & 0x00FF) == 0x0000)
    _ <- setFlag(CpuFlags.N, temp & 0x0080)
    _ <- address.write(temp & 0xFF)
  } yield Unit

  def RTI: Op = for {
    status1 <- pop
    status2 = status1 & ~CpuFlags.B.bit & ~CpuFlags.U.bit
    _ <- setStatus(status2)
    pc1 <- pop
    pc2 <- pop
    pc = asUInt16(pc2, pc1)
    _ <- setPc(pc)
  } yield Unit

  def RTS: Op = for {
    pc1 <- pop
    pc2 <- pop
    pc = asUInt16(pc2, pc1)
    _ <- setPc(pc)
    _ <- incPc
  } yield Unit

  def SBC(addressMode: AddressMode): Op = for {
    address <- addressMode
    d <- address.read()
    value = d ^ 0x00FF
    a <- getA
    c <- getFlag(CpuFlags.C)
    lsb = if (c) 1 else 0
    temp = a + value + lsb
    _ <- setFlag(CpuFlags.C, temp & 0xFF00)
    _ <- setFlag(CpuFlags.Z, (temp & 0x00FF) == 0x0000)
    _ <- setFlag(CpuFlags.V, (temp ^ a) & (temp ^ value) & 0x0080)
    _ <- setFlag(CpuFlags.N, temp & 0x00800)
    _ <- setA(temp & 0xFF)
  } yield Unit

  def SEC: Op = setFlag(CpuFlags.C, value = true)

  def SED: Op = setFlag(CpuFlags.D, value = true)

  def SEI: Op = setFlag(CpuFlags.I, value = true)

  def STA(addressMode: AddressMode): Op = for {
    address <- addressMode
    d <- getA
    _ <- address.write(d)
  } yield Unit

  def STX(addressMode: AddressMode): Op = for {
    address <- addressMode
    d <- getX
    _ <- address.write(d)
  } yield Unit

  def STY(addressMode: AddressMode): Op = for {
    address <- addressMode
    y <- getY
    _ <- address.write(y)
  } yield Unit

  def TAX: Op = for {
    d <- getA
    _ <- setX(d)
    _ <- setFlag(CpuFlags.Z, d == 0x00)
    _ <- setFlag(CpuFlags.N, d & 0x80)
  } yield Unit

  def TAY: Op = for {
    d <- getA
    _ <- setY(d)
    _ <- setFlag(CpuFlags.Z, d == 0x00)
    _ <- setFlag(CpuFlags.N, d & 0x80)
  } yield Unit

  def TSX: Op = for {
    d <- getStkp
    _ <- setX(d)
    _ <- setFlag(CpuFlags.Z, d == 0x00)
    _ <- setFlag(CpuFlags.N, d & 0x80)
  } yield Unit

  def TXA: Op = for {
    d <- getX
    _ <- setA(d)
    _ <- setFlag(CpuFlags.Z, d == 0x00)
    _ <- setFlag(CpuFlags.N, d & 0x80)
  } yield Unit

  def TXS: Op = for {
    d <- getX
    _ <- setStkp(d)
  } yield Unit

  def TYA: Op = for {
    d <- getY
    _ <- setA(d)
    _ <- setFlag(CpuFlags.Z, d == 0x00)
    _ <- setFlag(CpuFlags.N, d & 0x80)
  } yield Unit

  def XXX: Op = State.pure(Unit)

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
    0xE9 -> Instr("SBC/IMM", SBC(IMM), 2),     0xEA -> Instr("NOP/IMP", NOP(),    2),
    0xEC -> Instr("CPX/ABS", CPX(ABS), 4),     0xED -> Instr("SBC/ABS", SBC(ABS), 4),
    0xEE -> Instr("INC/ABS", INC(ABS), 6),     0xF0 -> Instr("BEQ/REL", BEQ,      2),
    0xF1 -> Instr("SBC/IZY", SBC(IZY), 5),     0xF5 -> Instr("SBC/ZPX", SBC(ZPX), 4),
    0xF6 -> Instr("INC/ZPX", INC(ZPX), 6),     0xF8 -> Instr("SED/IMP", SED,      2),
    0xF9 -> Instr("SBC/ABY", SBC(ABY), 4),     0xFD -> Instr("SBC/ABX", SBC(ABX), 4),
    0xFE -> Instr("INC/ABX", INC(ABX), 7)
  ).withDefault { d =>
    if (Set(0x1A, 0x3A, 0x5A, 0x7A, 0x80, 0x82, 0x89, 0xC2, 0xDA, 0xE2, 0xEA, 0xFA).contains(d))
      Instr("NOP/IMP", NOP(), 2)
    else if (Set(0x04, 0x44, 0x64).contains(d))
      Instr("NOP/IMP", NOP(), 3)
    else if (Set(0x0C, 0x14, 0x1C, 0x34, 0x3C, 0x54, 0x5C, 0x74, 0x7C, 0xD4, 0xDC, 0xF4, 0xFC).contains(d))
      Instr("NOP/IMP", NOP(), 4)
    else if (d == 0x8C)
      Instr("NOP/IMP", NOP(), 5)
    else if (Set(0x02, 0x12, 0x22, 0x32, 0x42, 0x52, 0x62, 0x72, 0x92, 0xB2, 0xD2, 0xF2, 0x0B, 0x2B, 0x4B, 0x6B, 0x8B, 0xAB, 0xCB).contains(d))
      Instr("XXX/IMP", XXX, 2)
    else if (Set(0x87, 0xA7).contains(d))
      Instr("XXX/IMP", XXX, 3)
    else if (Set(0x97, 0xBB, 0xB7, 0x8F, 0xAF, 0xBF).contains(d))
      Instr("XXX/IMP", XXX, 4)
    else if (Set(0xB3, 0x07, 0x27, 0x47, 0x67, 0xC7, 0xE7, 0x9B, 0x9E, 0x9F).contains(d))
      Instr("XXX/IMP", XXX, 5)
    else if (Set(0x83, 0x93, 0xA3, 0x17, 0x37, 0x57, 0x77, 0xD7, 0xF7, 0x0F, 0x2F, 0x4F, 0x6F, 0xCF, 0xEF).contains(d))
      Instr("XXX/IMP", XXX, 6)
    else if (Set(0x1B, 0x1F, 0x3B, 0x3F, 0x5B, 0x5F, 0x7B, 0x7F, 0xDB, 0xDF, 0xFB, 0xFF).contains(d))
      Instr("XXX/IMP", XXX, 7)
    else if (Set(0x03, 0x13, 0x23, 0x33, 0x43, 0x53, 0x63, 0x73, 0xC3, 0xD3, 0xE3, 0xF3).contains(d))
      Instr("XXX/IMP", XXX, 8)
    else {
      println(s"Invalid opcode: ${hex(d, 2)}")
      Instr("XXX/IMP", XXX, 8)
    }
  }

  def disassemble(start: UInt16, end: UInt16, nesState: NesState): Vector[(UInt16, String)] = {
    val (res, _) = Stream
      .range(start, end)
      .foldLeft((Queue.empty[(UInt16, String)], Queue.empty[UInt8])) { case ((acc, parts), address) =>
        val cmdParts = parts :+ nesState.ram(address)
        val infoParts = lookup(cmdParts.head).info.split('/')
        val opcode = infoParts.head
        val addressMode = infoParts.last
        if (addressMode == "IMP" && cmdParts.size == 1) {
          val cmd = (address - 0) -> s"$opcode {IMP}"
          (acc :+ cmd, Queue.empty)
        } else if (addressMode == "IMM" && cmdParts.size == 2) {
          val cmd = (address - 1) -> s"$opcode #$$${hex(cmdParts(1), 2)} {IMM}"
          (acc :+ cmd, Queue.empty)
        } else if (addressMode == "ZP0" && cmdParts.size == 2) {
          val cmd = (address - 1) -> s"$opcode $$${hex(cmdParts(1), 2)} {ZP0}"
          (acc :+ cmd, Queue.empty)
        } else if (addressMode == "ZPX" && cmdParts.size == 2) {
          val cmd = (address - 1) -> s"$opcode $$${hex(cmdParts(1), 2)}, X {ZPX}"
          (acc :+ cmd, Queue.empty)
        } else if (addressMode == "ZPY" && cmdParts.size == 2) {
          val cmd = (address - 1) -> s"$opcode $$${hex(cmdParts(1), 2)}, Y {ZPY}"
          (acc :+ cmd, Queue.empty)
        } else if (addressMode == "IZX" && cmdParts.size == 2) {
          val cmd = (address - 1) -> s"$opcode ($$${hex(cmdParts(1), 2)}, X) {IZX}"
          (acc :+ cmd, Queue.empty)
        } else if (addressMode == "IZY" && cmdParts.size == 2) {
          val cmd = (address - 1) -> s"$opcode ($$${hex(cmdParts(1), 2)}, Y) {IZY}"
          (acc :+ cmd, Queue.empty)
        } else if (addressMode == "ABS" && cmdParts.size == 3) {
          val a = asUInt16(cmdParts(2), cmdParts(1))
          val cmd = (address - 2) -> s"$opcode $$${hex(a, 4)} {ABS}"
          (acc :+ cmd, Queue.empty)
        } else if (addressMode == "ABX" && cmdParts.size == 3) {
          val a = asUInt16(cmdParts(2), cmdParts(1))
          val cmd = (address - 2) -> s"$opcode $$${hex(a, 4)}, X {ABX}"
          (acc :+ cmd, Queue.empty)
        } else if (addressMode == "ABY" && cmdParts.size == 3) {
          val a = asUInt16(cmdParts(2), cmdParts(1))
          val cmd = (address - 2) -> s"$opcode $$${hex(a, 4)}, Y {ABY}"
          (acc :+ cmd, Queue.empty)
        } else if (addressMode == "IND" && cmdParts.size == 3) {
          val a = asUInt16(cmdParts(2), cmdParts(1))
          val cmd = (address - 2) -> s"$opcode ($$${hex(a, 4)}) {IND}"
          (acc :+ cmd, Queue.empty)
        } else if (addressMode == "REL" && cmdParts.size == 2) {
          val a = cmdParts(1)
          val cmd = (address - 1) -> s"$opcode $$${hex(a, 2)} [$$${hex(address + 1 + a, 4)}] {REL}"
          (acc :+ cmd, Queue.empty)
        } else
          (acc, cmdParts)
      }
    res.toVector
  }
}
