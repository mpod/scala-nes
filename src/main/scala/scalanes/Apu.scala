package scalanes

class ApuState(
  var pulse1: PulseState.PulseChannel1,
  var pulse2: PulseState.PulseChannel2,
  var triangle: TriangleState,
  var noise: NoiseState,
  var dmc: DmcState,
  var frameCounterReg: UInt8
) {
  def frameCounterMode: Int         = (frameCounterReg >> 7) & 0x1
  def interruptInhibitFlag: Boolean = (frameCounterReg >> 6) & 0x1
}

object ApuState {
  val pulse1: Setter[ApuState, PulseState.PulseChannel1] = (a, s) => s.pulse1 = a
  val pulse2: Setter[ApuState, PulseState.PulseChannel2] = (a, s) => s.pulse2 = a
  val triangle: Setter[ApuState, TriangleState]          = (a, s) => s.triangle = a
  val noise: Setter[ApuState, NoiseState]                = (a, s) => s.noise = a
  val dmc: Setter[ApuState, DmcState]                    = (a, s) => s.dmc = a
  val frameCounterReg: Setter[ApuState, UInt8]           = (a, s) => s.frameCounterReg = a

  def apply(): ApuState =
    new ApuState(
      pulse1 = PulseState.channel1(),
      pulse2 = PulseState.channel2(),
      triangle = TriangleState(),
      noise = NoiseState(),
      dmc = DmcState(),
      frameCounterReg = 0x00
    )
}

class PulseState(
  var reg0: UInt8,
  var reg1: UInt8,
  var reg2: UInt8,
  var reg3: UInt8,
  var envelopeStart: Boolean,
  var envelopeDividerValue: UInt4,
  var envelopeDecayLevelCounter: UInt4,
  var sweepReload: Boolean,
  var sweepDividerValue: UInt3,
  var lengthCounterEnabled: Boolean,
  var lengthCounterValue: UInt4,
  var timerPeriod: UInt11,
  var timerValue: UInt11,
  var sequenceCounterValue: UInt4
) {
  def dutyMode: UInt2                = (reg0 >> 6) & 0x03
  def constantVolumeEnabled: Boolean = (reg0 >> 4) & 0x01
  def constantVolume: UInt4          = reg0 & 0x0f
  def envelopeDividerPeriod: UInt4   = reg0 & 0x0f
  def envelopeLoopEnabled: Boolean   = (reg0 >> 5) & 0x01
  def sweepEnabled: Boolean          = (reg1 >> 7) & 0x01
  def sweepDividerPeriod: UInt3      = (reg1 >> 4) & 0x07
  def sweepNegate: Boolean           = (reg1 >> 3) & 0x01
  def sweepShift: UInt3              = reg1 & 0x07
  def lengthCounterHalt: Boolean     = (reg0 >> 5) & 0x01
  def lengthCounterLoad: UInt5       = (reg3 >> 3) & 0x1f
}

object PulseState {
  val reg0: Setter[PulseState, UInt8]                      = (a, s) => s.reg0 = a
  val reg1: Setter[PulseState, UInt8]                      = (a, s) => s.reg1 = a
  val reg2: Setter[PulseState, UInt8]                      = (a, s) => s.reg2 = a
  val reg3: Setter[PulseState, UInt8]                      = (a, s) => s.reg3 = a
  val envelopeStart: Setter[PulseState, Boolean]           = (a, s) => s.envelopeStart = a
  val envelopeDividerValue: Setter[PulseState, UInt4]      = (a, s) => s.envelopeDividerValue = a
  val envelopeDecayLevelCounter: Setter[PulseState, UInt4] = (a, s) => s.envelopeDecayLevelCounter = a
  val sweepReload: Setter[PulseState, Boolean]             = (a, s) => s.sweepReload = a
  val sweepDividerValue: Setter[PulseState, UInt3]         = (a, s) => s.sweepDividerValue = a
  val lengthCounterEnabled: Setter[PulseState, Boolean]    = (a, s) => s.lengthCounterEnabled = a
  val lengthCounterValue: Setter[PulseState, UInt4]        = (a, s) => s.lengthCounterValue = a
  val timerPeriod: Setter[PulseState, UInt11]              = (a, s) => s.timerPeriod = a
  val timerValue: Setter[PulseState, UInt11]               = (a, s) => s.timerValue = a
  val sequenceCounterValue: Setter[PulseState, UInt4]      = (a, s) => s.sequenceCounterValue = a

  sealed trait Channel
  trait Channel1 extends Channel
  trait Channel2 extends Channel

  type PulseChannel  = PulseState with Channel
  type PulseChannel1 = PulseState with Channel1
  type PulseChannel2 = PulseState with Channel2

  def channel1(): PulseChannel1 =
    new PulseState(
      reg0 = 0x00,
      reg1 = 0x00,
      reg2 = 0x00,
      reg3 = 0x00,
      envelopeStart = false,
      envelopeDividerValue = 0,
      envelopeDecayLevelCounter = 0,
      sweepReload = false,
      sweepDividerValue = 0,
      lengthCounterEnabled = false,
      lengthCounterValue = 0,
      timerPeriod = 0,
      timerValue = 0,
      sequenceCounterValue = 0
    ) with Channel1

  def channel2(): PulseChannel2 =
    new PulseState(
      reg0 = 0x00,
      reg1 = 0x00,
      reg2 = 0x00,
      reg3 = 0x00,
      envelopeStart = false,
      envelopeDividerValue = 0,
      envelopeDecayLevelCounter = 0,
      sweepReload = false,
      sweepDividerValue = 0,
      lengthCounterEnabled = false,
      lengthCounterValue = 0,
      timerPeriod = 0,
      timerValue = 0,
      sequenceCounterValue = 0
    ) with Channel2

  def clockEnvelope[T <: PulseChannel](p: T): T =
    if (p.envelopeStart) {
      val p1 = envelopeStart.set(false)(p)
      val p2 = envelopeDecayLevelCounter.set(0xf)(p1)
      val p3 = envelopeDividerValue.set(p2.envelopeDividerPeriod)(p2)
      p3
    } else if (p.envelopeDividerValue > 0) {
      envelopeDividerValue.set(p.envelopeDividerValue - 1)(p)
    } else {
      val p1 = envelopeDividerValue.set(p.envelopeDividerPeriod)(p)
      val p2 =
        if (p1.envelopeDecayLevelCounter > 0)
          envelopeDecayLevelCounter.set(p1.envelopeDecayLevelCounter - 1)(p1)
        else if (p1.envelopeLoopEnabled)
          envelopeDecayLevelCounter.set(0xf)(p1)
        else
          p1
      p2
    }

  def clockSweep[T <: PulseChannel](p: T): T =
    if (p.sweepDividerValue == 0 || p.sweepReload) {
      val p1 = if (p.sweepDividerValue == 0 && p.sweepEnabled && p.timerValue > 8) {
        val changeAmount = p.timerPeriod >> p.sweepShift
        val targetPeriod = p.timerPeriod + (p match {
          case _: PulseChannel1 if p.sweepNegate => ~changeAmount & 0xffff
          case _: PulseChannel2 if p.sweepNegate => (~changeAmount + 1) & 0xffff
          case _                                 => changeAmount
        })
        timerPeriod.set(targetPeriod)(p)
      } else p
      val p2 = sweepDividerValue.set(p.sweepDividerPeriod)(p1)
      val p3 = sweepReload.set(false)(p2)
      p3
    } else
      sweepDividerValue.set(p.sweepDividerValue - 1)(p)

  def clockLengthCounter[T <: PulseChannel](p: T): T =
    if (p.lengthCounterValue > 0 && !p.lengthCounterHalt)
      lengthCounterValue.set(p.lengthCounterValue - 1)(p)
    else
      p

  def clockTimer[T <: PulseChannel](p: T): T =
    if (p.timerValue == 0) {
      val p1 = timerValue.set(p.timerPeriod)(p)
      val p2 = sequenceCounterValue.set((p.sequenceCounterValue + 1) % 8)(p1)
      p2
    } else
      timerValue.set(p.timerValue - 1)(p)
}

class TriangleState(
  var reg0: UInt8,
  var reg2: UInt8,
  var reg3: UInt8
) {
  def lengthCounterHalt: UInt1    = (reg0 >> 7) & 0x01
  def linearCounterControl: UInt1 = (reg0 >> 7) & 0x01
  def linearCounterLoad: UInt7    = reg0 & 0x7f
  def lengthCounterLoad: UInt5    = (reg3 >> 3) & 0x1f
  def timerPeriod: UInt11         = ((reg3 & 0x07) << 8) | reg2
}

object TriangleState {
  val reg0: Setter[TriangleState, UInt8] = (a, s) => s.reg0 = a
  val reg2: Setter[TriangleState, UInt8] = (a, s) => s.reg2 = a
  val reg3: Setter[TriangleState, UInt8] = (a, s) => s.reg3 = a

  def apply(): TriangleState = new TriangleState(0x00, 0x00, 0x00)
}

class NoiseState(
  var reg0: UInt8,
  var reg2: UInt8,
  var reg3: UInt8
) {
  def lengthCounterHalt: UInt1     = (reg0 >> 5) & 0x01
  def envelopeLoopEnabled: UInt1   = (reg0 >> 5) & 0x01
  def constantVolumeEnabled: UInt1 = (reg0 >> 4) & 0x01
  def constantVolume: UInt4        = reg0 & 0x0f
  def envelopeDividerPeriod: UInt4 = reg0 & 0x0f
  def loopNoiseEnabled: UInt1      = (reg2 >> 7) & 0x01
  def noisePeriod: UInt4           = reg2 & 0xf
  def lengthCounterLoad: UInt5     = (reg3 >> 3) & 0x1f
}

object NoiseState {
  val reg0: Setter[NoiseState, UInt8] = (a, s) => s.reg0 = a
  val reg2: Setter[NoiseState, UInt8] = (a, s) => s.reg2 = a
  val reg3: Setter[NoiseState, UInt8] = (a, s) => s.reg3 = a

  def apply(): NoiseState = new NoiseState(0x00, 0x00, 0x00)
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
}

object Apu {
  def setPulse1(pulse: PulseState.PulseChannel1)(nes: NesState): NesState = {
    val apu  = ApuState.pulse1.set(pulse)(nes.apuState)
    val nes1 = NesState.apuState.set(apu)(nes)
    nes1
  }

  def setPulse2(pulse: PulseState.PulseChannel2)(nes: NesState): NesState = {
    val apu  = ApuState.pulse2.set(pulse)(nes.apuState)
    val nes1 = NesState.apuState.set(apu)(nes)
    nes1
  }

  def setTriangle(triangle: TriangleState)(nes: NesState): NesState = {
    val apu  = ApuState.triangle.set(triangle)(nes.apuState)
    val nes1 = NesState.apuState.set(apu)(nes)
    nes1
  }

  def setNoise(noise: NoiseState)(nes: NesState): NesState = {
    val apu  = ApuState.noise.set(noise)(nes.apuState)
    val nes1 = NesState.apuState.set(apu)(nes)
    nes1
  }

  def setDmc(dmc: DmcState)(nes: NesState): NesState = {
    val apu  = ApuState.dmc.set(dmc)(nes.apuState)
    val nes1 = NesState.apuState.set(apu)(nes)
    nes1
  }

  def setFrameCounterReg(d: UInt8)(nes: NesState): NesState = {
    val apu  = ApuState.frameCounterReg.set(d)(nes.apuState)
    val nes1 = NesState.apuState.set(apu)(nes)
    nes1
  }

  def cpuWrite(address: UInt16, d: UInt8): NesState => NesState =
    nes => {
      require(address >= 0x4000 && address <= 0x4017 && address != 0x4014, f"Invalid address $address%#04x")
      require((d & 0xff) == d)
      address match {
        case 0x4000 =>
          setPulse1(PulseState.reg0.set(d)(nes.apuState.pulse1))(nes)
        case 0x4001 =>
          setPulse1(PulseState.reg1.set(d)(nes.apuState.pulse1))(nes)
        case 0x4002 =>
          val p  = nes.apuState.pulse1
          val p1 = PulseState.reg2.set(d)(p)
          val p2 = PulseState.timerPeriod.set(((p1.reg3 & 0x07) << 8) | d)(p1)
          setPulse1(p2)(nes)
        case 0x4003 =>
          val p  = nes.apuState.pulse1
          val p1 = PulseState.reg3.set(d)(p)
          val p2 = PulseState.timerPeriod.set(((d & 0x07) << 8) | p1.reg2)(p1)
          setPulse1(p2)(nes)
        case 0x4004 =>
          setPulse2(PulseState.reg0.set(d)(nes.apuState.pulse2))(nes)
        case 0x4005 =>
          setPulse2(PulseState.reg1.set(d)(nes.apuState.pulse2))(nes)
        case 0x4006 =>
          val p  = nes.apuState.pulse2
          val p1 = PulseState.reg2.set(d)(p)
          val p2 = PulseState.timerPeriod.set(((p1.reg3 & 0x07) << 8) | d)(p1)
          setPulse2(p2)(nes)
        case 0x4007 =>
          val p  = nes.apuState.pulse2
          val p1 = PulseState.reg3.set(d)(p)
          val p2 = PulseState.timerPeriod.set(((d & 0x07) << 8) | p1.reg2)(p1)
          setPulse2(p2)(nes)
        case 0x4008 =>
          setTriangle(TriangleState.reg0.set(d)(nes.apuState.triangle))(nes)
        case 0x4009 =>
          nes
        case 0x400a =>
          setTriangle(TriangleState.reg2.set(d)(nes.apuState.triangle))(nes)
        case 0x400b =>
          setTriangle(TriangleState.reg3.set(d)(nes.apuState.triangle))(nes)
        case 0x400c =>
          setNoise(NoiseState.reg0.set(d)(nes.apuState.noise))(nes)
        case 0x400d =>
          nes
        case 0x400e =>
          setNoise(NoiseState.reg2.set(d)(nes.apuState.noise))(nes)
        case 0x400f =>
          setNoise(NoiseState.reg3.set(d)(nes.apuState.noise))(nes)
        case 0x4010 =>
          setDmc(DmcState.reg0.set(d)(nes.apuState.dmc))(nes)
        case 0x4011 =>
          setDmc(DmcState.reg1.set(d)(nes.apuState.dmc))(nes)
        case 0x4012 =>
          setDmc(DmcState.reg2.set(d)(nes.apuState.dmc))(nes)
        case 0x4013 =>
          setDmc(DmcState.reg3.set(d)(nes.apuState.dmc))(nes)
        case 0x4015 =>
          val nes1 = setPulse1(PulseState.lengthCounterEnabled.set(d & 0x01)(nes.apuState.pulse1))(nes)
          val nes2 = setPulse2(PulseState.lengthCounterEnabled.set(d & 0x02)(nes.apuState.pulse2))(nes1)
          nes2
        case 0x4017 =>
          setFrameCounterReg(d)(nes)
        case _ => throw new RuntimeException(f"Invalid cpu memory write at address $address%#04x")
      }
    }

  def clock(nes: NesState): NesState = {
    val nes1 =
      if (nes.apuState.frameCounterMode == 0)
        nes.cpuState.cycles % 29830 match {
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
        }
      else
        nes.cpuState.cycles % 37282 match {
          case 7457 | 22371 =>
            clockEnvelope(nes)
          case 14913 | 37281 =>
            val nes1 = clockEnvelope(nes)
            val nes2 = clockLengthCounter(nes1)
            clockSweep(nes2)
        }
    clockTimer(nes1)
  }

  def clockEnvelope(nes: NesState): NesState = {
    val apu  = nes.apuState
    val apu1 = ApuState.pulse1.set(PulseState.clockEnvelope(apu.pulse1))(apu)
    val apu2 = ApuState.pulse2.set(PulseState.clockEnvelope(apu.pulse2))(apu1)
    NesState.apuState.set(apu2)(nes)
  }

  def clockSweep(nes: NesState): NesState = {
    val apu  = nes.apuState
    val apu1 = ApuState.pulse1.set(PulseState.clockSweep(apu.pulse1))(apu)
    val apu2 = ApuState.pulse2.set(PulseState.clockSweep(apu.pulse2))(apu1)
    NesState.apuState.set(apu2)(nes)
  }

  def clockLengthCounter(nes: NesState): NesState = {
    val apu  = nes.apuState
    val apu1 = ApuState.pulse1.set(PulseState.clockLengthCounter(apu.pulse1))(apu)
    val apu2 = ApuState.pulse2.set(PulseState.clockLengthCounter(apu.pulse2))(apu1)
    NesState.apuState.set(apu2)(nes)
  }

  def clockTimer(nes: NesState): NesState =
    if (nes.cpuState.cycles % 2 == 0) {
      val apu  = nes.apuState
      val apu1 = ApuState.pulse1.set(PulseState.clockTimer(apu.pulse1))(apu)
      val apu2 = ApuState.pulse2.set(PulseState.clockTimer(apu.pulse2))(apu1)
      NesState.apuState.set(apu2)(nes)
    } else nes

  def triggerIrq(nes: NesState): NesState = nes
}
