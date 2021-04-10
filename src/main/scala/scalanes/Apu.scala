package scalanes

import cats._
import cats.implicits._

import scala.collection.mutable.ArrayBuffer

class ApuState(
  var pulse1: PulseState,
  var pulse2: PulseState,
  var triangle: TriangleState,
  var noise: NoiseState,
  var dmc: DmcState,
  var frameCounterReg: UInt8,
  val filter: Filter,
  var cycles: Long,
  val buffer: ArrayBuffer[Byte]
) {
  def frameCounterMode: Int         = (frameCounterReg >> 7) & 0x1
  def interruptInhibitFlag: Boolean = (frameCounterReg >> 6) & 0x1
  def flush: Array[Byte] = {
    val array = buffer.toArray
    buffer.clear()
    array
  }
}

object ApuState {
  val pulse1: Setter[ApuState, PulseState]      = (a, s) => s.pulse1 = a
  val pulse2: Setter[ApuState, PulseState]      = (a, s) => s.pulse2 = a
  val triangle: Setter[ApuState, TriangleState] = (a, s) => s.triangle = a
  val noise: Setter[ApuState, NoiseState]       = (a, s) => s.noise = a
  val dmc: Setter[ApuState, DmcState]           = (a, s) => s.dmc = a
  val frameCounterReg: Setter[ApuState, UInt8]  = (a, s) => s.frameCounterReg = a
  val cycles: Setter[ApuState, Long]            = (a, s) => s.cycles = a

  def apply(): ApuState =
    new ApuState(
      pulse1 = PulseState.channel(),
      pulse2 = PulseState.channel(),
      triangle = TriangleState(),
      noise = NoiseState(),
      dmc = DmcState(),
      frameCounterReg = 0x00,
      filter = FilterChain.default,
      cycles = 0L,
      buffer = ArrayBuffer.empty
    )
}

trait Channel {
  var reg0: UInt8                   = 0
  var reg1: UInt8                   = 0
  var reg2: UInt8                   = 0
  var reg3: UInt8                   = 0
  var lengthCounterEnabled: Boolean = false
  var lengthCounterValue: Int       = 0
  def lengthCounterHalt: Boolean
  def lengthCounterLoad: UInt5 = (reg3 >> 3) & 0x1f
}

object Channel {
  val reg0: Setter[Channel, UInt8]                   = (a, s) => s.reg0 = a
  val reg1: Setter[Channel, UInt8]                   = (a, s) => s.reg1 = a
  val reg2: Setter[Channel, UInt8]                   = (a, s) => s.reg2 = a
  val reg3: Setter[Channel, UInt8]                   = (a, s) => s.reg3 = a
  val lengthCounterEnabled: Setter[Channel, Boolean] = (a, s) => s.lengthCounterEnabled = a
  val lengthCounterValue: Setter[Channel, Int]       = (a, s) => s.lengthCounterValue = a

  def disableLengthCounter[T <: Channel](c: T): Id[T] =
    c.pure[Id]
      .map(c => lengthCounterEnabled.set(false)(c))
      .map(c => lengthCounterValue.set(0)(c))

  def enableLengthCounter[T <: Channel](c: T): T =
    lengthCounterEnabled.set(true)(c)

  def clockLengthCounter[T <: Channel](c: T): T =
    if (c.lengthCounterValue > 0 && !c.lengthCounterHalt)
      lengthCounterValue.set(c.lengthCounterValue - 1)(c)
    else
      c
}

trait Envelope extends Channel {
  var envelopeStart: Boolean           = 0
  var envelopeDividerValue: UInt4      = 0
  var envelopeDecayLevelCounter: UInt4 = 0
  def constantVolumeEnabled: Boolean   = (reg0 >> 4) & 0x01
  def constantVolume: UInt4            = reg0 & 0x0f
  def envelopeDividerPeriod: UInt4     = reg0 & 0x0f
  def envelopeLoopEnabled: Boolean     = (reg0 >> 5) & 0x01
}

object Envelope {
  val envelopeStart: Setter[Envelope, Boolean]           = (a, s) => s.envelopeStart = a
  val envelopeDividerValue: Setter[Envelope, UInt4]      = (a, s) => s.envelopeDividerValue = a
  val envelopeDecayLevelCounter: Setter[Envelope, UInt4] = (a, s) => s.envelopeDecayLevelCounter = a

  def clockEnvelope[T <: Envelope](e: T): Id[T] =
    if (e.envelopeStart)
      e.pure[Id]
        .map(p => envelopeStart.set(false)(p))
        .map(p => envelopeDecayLevelCounter.set(0xf)(p))
        .map(p => envelopeDividerValue.set(p.envelopeDividerPeriod)(p))
    else if (e.envelopeDividerValue > 0)
      envelopeDividerValue.set(e.envelopeDividerValue - 1)(e)
    else
      e.pure[Id]
        .map(p => envelopeDividerValue.set(p.envelopeDividerPeriod)(p))
        .map { p =>
          if (p.envelopeDecayLevelCounter || p.envelopeLoopEnabled)
            envelopeDecayLevelCounter.set((p.envelopeDecayLevelCounter - 1) & 0xff)(p)
          else
            p
        }
}

class PulseState(
  var sweepReload: Boolean,
  var sweepDividerValue: UInt3,
  var timerValue: UInt11,
  var sequenceCounterValue: UInt4
) extends Envelope {
  def dutyMode: UInt2            = (reg0 >> 6) & 0x03
  def sweepEnabled: Boolean      = (reg1 >> 7) & 0x01
  def sweepDividerPeriod: UInt3  = ((reg1 >> 4) & 0x07) + 1
  def sweepNegate: Boolean       = (reg1 >> 3) & 0x01
  def sweepShift: UInt3          = reg1 & 0x07
  def lengthCounterHalt: Boolean = (reg0 >> 5) & 0x01
  def timerPeriod: UInt11        = ((reg3 & 0x7) << 8) | (reg2 & 0xff)
}

object PulseState {
  val sweepReload: Setter[PulseState, Boolean]        = (a, s) => s.sweepReload = a
  val sweepDividerValue: Setter[PulseState, UInt3]    = (a, s) => s.sweepDividerValue = a
  val timerValue: Setter[PulseState, UInt11]          = (a, s) => s.timerValue = a
  val sequenceCounterValue: Setter[PulseState, UInt4] = (a, s) => s.sequenceCounterValue = a
  val timerPeriod: Setter[PulseState, UInt11] = (a, s) => {
    s.reg2 = a & 0xff
    s.reg3 = (s.reg3 & ~0x7) | ((a >> 8) & 0x7)
  }

  val dutySequences: Vector[Vector[Int]] =
    Vector(
      Vector(0, 1, 0, 0, 0, 0, 0, 0),
      Vector(0, 1, 1, 0, 0, 0, 0, 0),
      Vector(0, 1, 1, 1, 1, 0, 0, 0),
      Vector(1, 0, 0, 1, 1, 1, 1, 1)
    )

  def channel(): PulseState =
    new PulseState(
      sweepReload = false,
      sweepDividerValue = 0,
      timerValue = 0,
      sequenceCounterValue = 0
    )

  def clockSweep(negativeAdjust: Int)(p: PulseState): PulseState =
    p.pure[Id]
      .map(p => sweepDividerValue.set(p.sweepDividerValue - 1)(p))
      .map(p => if (p.sweepDividerValue < 0) sweepReload.set(true)(p) else p)
      .map { p =>
        if (p.sweepDividerValue < 0 && p.sweepShift && p.sweepEnabled && p.timerPeriod >= 8) {
          val shiftedPeriod = p.timerPeriod >> p.sweepShift
          val offset        = if (p.sweepNegate) negativeAdjust - shiftedPeriod else shiftedPeriod
          if (p.timerPeriod + offset < 0x800) {
            val period = p.timerPeriod + offset
            timerPeriod.set(period)(p)
          } else p
        } else p
      }
      .map(p => if (p.sweepReload) sweepDividerValue.set(p.sweepDividerPeriod)(p) else p)
      .map(p => sweepReload.set(false)(p))

  def clockTimer(p: PulseState): PulseState =
    p.pure[Id]
      .map(p => timerValue.set(p.timerValue - 1)(p))
      .map(p => if (p.timerValue < 0) sequenceCounterValue.set((p.sequenceCounterValue + 1) % 8)(p) else p)
      .map(p => if (p.timerValue < 0) timerValue.set(p.timerPeriod)(p) else p)

  def output(p: PulseState): Int =
    if (!p.lengthCounterEnabled) 0
    else if (p.lengthCounterValue == 0) 0
    else if (dutySequences(p.dutyMode)(p.sequenceCounterValue) == 0) 0
    else if (p.timerPeriod < 8 || p.timerPeriod > 0x7ff) 0
    else if (!p.constantVolumeEnabled) p.envelopeDecayLevelCounter
    else p.constantVolume
}

class TriangleState(
  var linearCounterReload: Boolean,
  var linearCounterValue: UInt7,
  var timerValue: UInt11,
  var sequenceCounterValue: Int
) extends Channel {
  def lengthCounterHalt: Boolean    = (reg0 >> 7) & 0x01
  def linearCounterControl: Boolean = (reg0 >> 7) & 0x01
  def linearCounterLoad: UInt7      = reg0 & 0x7f
  def timerPeriod: UInt11           = ((reg3 & 0x07) << 8) | reg2
}

object TriangleState {
  val linearCounterReload: Setter[TriangleState, Boolean] = (a, s) => s.linearCounterReload = a
  val linearCounterValue: Setter[TriangleState, UInt7]    = (a, s) => s.linearCounterValue = a
  val timerValue: Setter[TriangleState, UInt11]           = (a, s) => s.timerValue = a
  val sequenceCounterValue: Setter[TriangleState, Int]    = (a, s) => s.sequenceCounterValue = a

  val triangleSequence: Vector[Int] =
    Vector(
      // format: off
      15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0,
      0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15
      // format: on
    )

  def apply(): TriangleState =
    new TriangleState(
      linearCounterReload = false,
      linearCounterValue = 0,
      timerValue = 0,
      sequenceCounterValue = 0
    )

  def clockLinearCounter(t: TriangleState): TriangleState =
    t.pure[Id]
      .map { t =>
        val v = if (t.linearCounterReload) t.linearCounterLoad else t.linearCounterValue - 1
        linearCounterValue.set(v)(t)
      }
      .map { t =>
        if (!t.linearCounterControl) linearCounterReload.set(false)(t)
        else t
      }

  def clockTimer(t: TriangleState): TriangleState =
    t.pure[Id]
      .map { t =>
        if (t.timerValue == 0 && t.lengthCounterValue > 0 && t.linearCounterValue > 0)
          sequenceCounterValue.set((t.sequenceCounterValue + 1) % 32)(t)
        else
          t
      }
      .map { t =>
        if (t.timerValue == 0) timerValue.set(t.timerPeriod)(t)
        else t
      }

  def output(t: TriangleState): Int =
    if (!t.lengthCounterEnabled) 0
    else if (t.lengthCounterValue == 0) 0
    else if (t.linearCounterValue == 0) 0
    else triangleSequence(t.sequenceCounterValue)
}

class NoiseState(
  var shiftRegister: UInt15,
  var timerValue: Int
) extends Envelope {
  def lengthCounterHalt: Boolean = (reg0 >> 5) & 0x01
  def mode: Boolean              = (reg2 >> 7) & 0x01
  def timerPeriod: Int           = NoiseState.timerPeriods(reg2 & 0xf)
}

object NoiseState {
  val shiftRegister: Setter[NoiseState, UInt15] = (a, s) => s.shiftRegister = a
  val timerValue: Setter[NoiseState, Int]       = (a, s) => s.timerValue = a

  val timerPeriods: Vector[Int] = Vector(4, 8, 16, 32, 64, 96, 128, 160, 202, 254, 380, 508, 762, 1016, 2034, 4068)

  def apply(): NoiseState = new NoiseState(1, 0)

  def clockTimer(n: NoiseState): NoiseState =
    n.pure[Id]
      .map { n =>
        if (n.timerValue == 0) {
          val shift    = if (n.mode) 6 else 1
          val feedback = (n.shiftRegister & 0x1) ^ ((n.shiftRegister >> shift) & 0x1)
          val v        = (n.shiftRegister >> 1) | (feedback << 14)
          NoiseState.shiftRegister.set(v)(n)
        } else n
      }
      .map { n =>
        if (n.timerValue == 0) NoiseState.timerValue.set(n.timerPeriod)(n)
        else NoiseState.timerValue.set(n.timerValue - 1)(n)
      }

  def output(n: NoiseState): Int =
    if (!n.lengthCounterEnabled) 0
    else if (n.lengthCounterValue == 0) 0
    else if (n.shiftRegister & 1 == 1) 0
    else if (!n.constantVolumeEnabled) n.envelopeDecayLevelCounter
    else n.constantVolume
}

class DmcState(
  var reg0: UInt8,
  var reg1: UInt8,
  var reg2: UInt8,
  var reg3: UInt8
) {}

object DmcState {
  val reg0: Setter[DmcState, UInt8] = (a, s) => s.reg0 = a
  val reg1: Setter[DmcState, UInt8] = (a, s) => s.reg1 = a
  val reg2: Setter[DmcState, UInt8] = (a, s) => s.reg2 = a
  val reg3: Setter[DmcState, UInt8] = (a, s) => s.reg3 = a

  def apply(): DmcState = new DmcState(0x00, 0x00, 0x00, 0x00)

  def output(d: DmcState): Int = 0
}

trait Filter {
  def step(x: Float): Float
}

class FirstOrderFilter(
  var b0: Float,
  var b1: Float,
  var a1: Float,
  var prevX: Float,
  var prevY: Float
) extends Filter {
  override def step(x: Float): Float = {
    val y = b0 * x + b1 * prevX - a1 * prevY
    prevY = y
    prevX = x
    y
  }
}

object FirstOrderFilter {
  def lowPassFilter(sampleRate: Float, cutoffFreq: Float): FirstOrderFilter = {
    val c  = sampleRate / math.Pi.toFloat / cutoffFreq
    val a0 = 1 / (1 + c)
    new FirstOrderFilter(b0 = a0, b1 = a0, a1 = (1 - c) * a0, 0, 0)
  }

  def highPassFilter(sampleRate: Float, cutoffFreq: Float): FirstOrderFilter = {
    val c  = sampleRate / math.Pi.toFloat / cutoffFreq
    val a0 = 1 / (1 + c)
    new FirstOrderFilter(b0 = c * a0, b1 = -c * a0, a1 = (1 - c) * a0, 0, 0)
  }
}

class FilterChain(filters: List[Filter]) extends Filter {
  override def step(x: Float): Float =
    filters.foldLeft(x)((x, filter) => filter.step(x))
}

object FilterChain {
  def default: FilterChain = {
    val sampleRate = 44100
    new FilterChain(
      List(
        FirstOrderFilter.highPassFilter(sampleRate, 90),
        FirstOrderFilter.highPassFilter(sampleRate, 440),
        FirstOrderFilter.lowPassFilter(sampleRate, 1400)
      )
    )
  }
}

object Apu {
  val pulseTable: Vector[Float] = (0 to 30).map(i => 95.52f / (8128.0f / i + 100)).toVector
  val tndTable: Vector[Float]   = (0 to 202).map(i => 163.67f / (24329.0f / i + 100)).toVector

  val lengthCounterTable: Vector[UInt8] = Vector(
    // format: off
    10,254, 20,  2, 40,  4, 80,  6, 160,  8, 60, 10, 14, 12, 26, 14,
    12, 16, 24, 18, 48, 20, 96, 22, 192, 24, 72, 26, 16, 28, 32, 30
    // format: on
  )

  def setPulse1(pulse: PulseState)(nes: NesState): NesState =
    nes.apuState
      .pure[Id]
      .map(apu => ApuState.pulse1.set(pulse)(apu))
      .map(apu => NesState.apuState.set(apu)(nes))

  def setPulse2(pulse: PulseState)(nes: NesState): NesState =
    nes.apuState
      .pure[Id]
      .map(apu => ApuState.pulse2.set(pulse)(apu))
      .map(apu => NesState.apuState.set(apu)(nes))

  def setTriangle(triangle: TriangleState)(nes: NesState): NesState =
    nes.apuState
      .pure[Id]
      .map(apu => ApuState.triangle.set(triangle)(apu))
      .map(apu => NesState.apuState.set(apu)(nes))

  def setNoise(noise: NoiseState)(nes: NesState): NesState =
    nes.apuState
      .pure[Id]
      .map(apu => ApuState.noise.set(noise)(apu))
      .map(apu => NesState.apuState.set(apu)(nes))

  def setDmc(dmc: DmcState)(nes: NesState): NesState =
    nes.apuState
      .pure[Id]
      .map(apu => ApuState.dmc.set(dmc)(apu))
      .map(apu => NesState.apuState.set(apu)(nes))

  def setFrameCounterReg(d: UInt8)(nes: NesState): NesState =
    nes.apuState
      .pure[Id]
      .map(apu => ApuState.cycles.set(0)(apu))
      .map(apu => ApuState.frameCounterReg.set(d)(apu))
      .map(apu => NesState.apuState.set(apu)(nes))

  def cpuWrite(address: UInt16, d: UInt8): NesState => NesState =
    nes => {
      require(address >= 0x4000 && address <= 0x4017 && address != 0x4014, f"Invalid address $address%#04x")
      require((d & 0xff) == d)
      address match {
        case 0x4000 =>
          setPulse1(Channel.reg0.set(d)(nes.apuState.pulse1))(nes)
        case 0x4001 =>
          nes.apuState.pulse1
            .pure[Id]
            .map(p => PulseState.sweepReload.set(true)(p))
            .map(p => setPulse1(Channel.reg1.set(d)(p))(nes))
        case 0x4002 =>
          setPulse1(Channel.reg2.set(d)(nes.apuState.pulse1))(nes)
        case 0x4003 =>
          nes.apuState.pulse1
            .pure[Id]
            .map(p => Channel.reg3.set(d)(p))
            .map(p => Envelope.envelopeStart.set(true)(p))
            .map(p => Channel.lengthCounterValue.set(lengthCounterTable((d >> 3) & 0x1f))(p))
            .map(p => PulseState.sequenceCounterValue.set(0)(p))
            .map(p => setPulse1(p)(nes))
        case 0x4004 =>
          setPulse2(Channel.reg0.set(d)(nes.apuState.pulse2))(nes)
        case 0x4005 =>
          nes.apuState.pulse2
            .pure[Id]
            .map(p => PulseState.sweepReload.set(true)(p))
            .map(p => setPulse1(Channel.reg1.set(d)(p))(nes))
        case 0x4006 =>
          setPulse2(Channel.reg2.set(d)(nes.apuState.pulse2))(nes)
        case 0x4007 =>
          nes.apuState.pulse2
            .pure[Id]
            .map(p => Channel.reg3.set(d)(p))
            .map(p => Envelope.envelopeStart.set(true)(p))
            .map(p => Channel.lengthCounterValue.set(lengthCounterTable((d >> 3) & 0x1f))(p))
            .map(p => PulseState.sequenceCounterValue.set(0)(p))
            .map(p => setPulse2(p)(nes))
        case 0x4008 =>
          setTriangle(Channel.reg0.set(d)(nes.apuState.triangle))(nes)
        case 0x4009 =>
          nes
        case 0x400a =>
          setTriangle(Channel.reg2.set(d)(nes.apuState.triangle))(nes)
        case 0x400b =>
          nes.apuState.triangle
            .pure[Id]
            .map(t => Channel.reg3.set(d)(t))
            .map(t => TriangleState.linearCounterReload.set(true)(t))
            .map(t => Channel.lengthCounterValue.set(lengthCounterTable((d >> 3) & 0x1f))(t))
            .map(t => setTriangle(t)(nes))
        case 0x400c =>
          setNoise(Channel.reg0.set(d)(nes.apuState.noise))(nes)
        case 0x400d =>
          nes
        case 0x400e =>
          setNoise(Channel.reg2.set(d)(nes.apuState.noise))(nes)
        case 0x400f =>
          nes.apuState.noise
            .pure[Id]
            .map(n => Envelope.envelopeStart.set(true)(n))
            .map(n => Channel.lengthCounterValue.set(lengthCounterTable((d >> 3) & 0x1f))(n))
            .map(n => setNoise(n)(nes))
        case 0x4010 =>
          setDmc(DmcState.reg0.set(d)(nes.apuState.dmc))(nes)
        case 0x4011 =>
          setDmc(DmcState.reg1.set(d)(nes.apuState.dmc))(nes)
        case 0x4012 =>
          setDmc(DmcState.reg2.set(d)(nes.apuState.dmc))(nes)
        case 0x4013 =>
          setDmc(DmcState.reg3.set(d)(nes.apuState.dmc))(nes)
        case 0x4015 =>
          nes.apuState
            .pure[Id]
            .map { apu =>
              ApuState.pulse1.set(
                if (d & 0x01) Channel.enableLengthCounter(apu.pulse1)
                else Channel.disableLengthCounter(apu.pulse1)
              )(apu)
            }
            .map { apu =>
              ApuState.pulse2.set(
                if (d & 0x02) Channel.enableLengthCounter(apu.pulse2)
                else Channel.disableLengthCounter(apu.pulse2)
              )(apu)
            }
            .map { apu =>
              ApuState.triangle.set(
                if (d & 0x04) Channel.enableLengthCounter(apu.triangle)
                else Channel.disableLengthCounter(apu.triangle)
              )(apu)
            }
            .map { apu =>
              ApuState.noise.set(
                if (d & 0x08) Channel.enableLengthCounter(apu.noise)
                else Channel.disableLengthCounter(apu.noise)
              )(apu)
            }
            .map(apu => NesState.apuState.set(apu)(nes))
        case 0x4017 =>
          setFrameCounterReg(d)(nes)
        case _ => throw new RuntimeException(f"Invalid cpu memory write at address $address%#04x")
      }
    }

  def cpuRead(address: UInt16): State[NesState, UInt8] =
    nes => {
      require(address == 0x4015)
      val apu            = nes.apuState
      val pulse1Status   = if (apu.pulse1.lengthCounterValue > 0) 1 else 0
      val pulse2Status   = if (apu.pulse2.lengthCounterValue > 0) 2 else 0
      val triangleStatus = 0
      val noiseStatus    = 0
      val dmcStatus      = 0
      (nes, dmcStatus | noiseStatus | triangleStatus | pulse2Status | pulse1Status)
    }

  def clock(nes: NesState): NesState = {
    val nes1 =
      if (nes.apuState.frameCounterMode == 0)
        nes.apuState.cycles % 29830 match {
          case 7457 | 22371 =>
            clockEnvelope(nes)
          case 14913 =>
            val nes1 = clockEnvelope(nes)
            val nes2 = clockLengthCounter(nes1)
            clockSweep(nes2)
          case 29829 =>
            val nes1 = clockEnvelope(nes)
            val nes2 = clockLengthCounter(nes1)
            val nes3 = clockSweep(nes2)
            triggerIrq(nes3)
          case _ =>
            nes
        }
      else
        nes.apuState.cycles % 37282 match {
          case 7457 | 22371 =>
            clockEnvelope(nes)
          case 14913 | 37281 =>
            val nes1 = clockEnvelope(nes)
            val nes2 = clockLengthCounter(nes1)
            clockSweep(nes2)
          case _ =>
            nes
        }
    val nes2 = clockTimer(nes1)
    if (nes2.apuState.cycles % 40 == 0) {
      val output = (255 * nes2.apuState.filter.step(mix(nes2))).toByte
      nes2.apuState.buffer += output
    }
    incCycles(nes2)
  }

  def clockEnvelope(nes: NesState): NesState =
    nes.apuState
      .pure[Id]
      .map(apu => ApuState.pulse1.set(Envelope.clockEnvelope(apu.pulse1))(apu))
      .map(apu => ApuState.pulse2.set(Envelope.clockEnvelope(apu.pulse2))(apu))
      .map(apu => ApuState.noise.set(Envelope.clockEnvelope(apu.noise))(apu))
      .map(apu => NesState.apuState.set(apu)(nes))

  def clockSweep(nes: NesState): NesState =
    nes.apuState
      .pure[Id]
      .map(apu => ApuState.pulse1.set(PulseState.clockSweep(-1)(apu.pulse1))(apu))
      .map(apu => ApuState.pulse2.set(PulseState.clockSweep(0)(apu.pulse2))(apu))
      .map(apu => NesState.apuState.set(apu)(nes))

  def clockLengthCounter(nes: NesState): NesState =
    nes.apuState
      .pure[Id]
      .map(apu => ApuState.pulse1.set(Channel.clockLengthCounter(apu.pulse1))(apu))
      .map(apu => ApuState.pulse2.set(Channel.clockLengthCounter(apu.pulse2))(apu))
      .map(apu => ApuState.triangle.set(Channel.clockLengthCounter(apu.triangle))(apu))
      .map(apu => ApuState.noise.set(Channel.clockLengthCounter(apu.noise))(apu))
      .map(apu => NesState.apuState.set(apu)(nes))

  def clockTimer(nes: NesState): NesState = {
    val apu =
      if (nes.cpuState.cycles % 2 == 0)
        nes.apuState
          .pure[Id]
          .map(apu => ApuState.pulse1.set(PulseState.clockTimer(apu.pulse1))(apu))
          .map(apu => ApuState.pulse2.set(PulseState.clockTimer(apu.pulse2))(apu))
          .map(apu => ApuState.noise.set(NoiseState.clockTimer(apu.noise))(apu))
      else
        nes.apuState.pure[Id]
    apu
      .map(apu => ApuState.triangle.set(TriangleState.clockTimer(apu.triangle))(apu))
      .map(apu => NesState.apuState.set(apu)(nes))
  }

  def mix(nes: NesState): Float = {
    val apu = nes.apuState
    val p1  = PulseState.output(apu.pulse1)
    val p2  = PulseState.output(apu.pulse2)
    val t   = TriangleState.output(apu.triangle)
    val n   = NoiseState.output(apu.noise)
    val d   = DmcState.output(apu.dmc)
    pulseTable(p1 + p2) + tndTable(3 * t + 2 * n + d)
  }

  def triggerIrq(nes: NesState): NesState = nes

  def incCycles(nes: NesState): NesState = {
    val apu  = nes.apuState
    val apu1 = ApuState.cycles.set(apu.cycles + 1)(apu)
    NesState.apuState.set(apu1)(nes)
  }
}
