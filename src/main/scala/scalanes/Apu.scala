package scalanes

class ApuState(
  var pulse1: PulseState.Channel1,
  var pulse2: PulseState.Channel2,
  var triangle: TriangleState,
  var noise: NoiseState,
  var dmc: DmcState
)

object ApuState {
  val pulse1: Setter[ApuState, PulseState.Channel1] = (a, s) => s.pulse1 = a
  val pulse2: Setter[ApuState, PulseState.Channel2] = (a, s) => s.pulse2 = a
  val triangle: Setter[ApuState, TriangleState]     = (a, s) => s.triangle = a
  val noise: Setter[ApuState, NoiseState]           = (a, s) => s.noise = a
  val dmc: Setter[ApuState, DmcState]               = (a, s) => s.dmc = a

  def apply(): ApuState =
    new ApuState(
      pulse1 = PulseState.channel1(),
      pulse2 = PulseState.channel2(),
      triangle = TriangleState(),
      noise = NoiseState(),
      dmc = DmcState()
    )
}

class PulseState(
  var reg0: UInt8,
  var reg1: UInt8,
  var reg2: UInt8,
  var reg3: UInt8,
  var lengthCounterEnabled: UInt1,
  var lengthCounterValue: UInt4,
  var timerValue: UInt11,
  var envelopeStart: UInt1,
  var sweepReload: UInt1
) {
  def dutyMode: UInt2              = (reg0 >> 6) & 0x03
  def lengthCounterHalt: UInt1     = (reg0 >> 5) & 0x01
  def constantVolumeEnabled: UInt1 = (reg0 >> 4) & 0x01
  def constantVolume: UInt4        = reg0 & 0x0f
  def envelopeDividerPeriod: UInt4 = reg0 & 0x0f
  def envelopeLoopEnabled: UInt1   = (reg0 >> 5) & 0x01
  def sweepEnabled: UInt1          = (reg1 >> 7) & 0x01
  def sweepPeriod: UInt3           = (reg1 >> 4) & 0x07
  def sweepNegate: UInt1           = (reg1 >> 3) & 0x01
  def sweepShift: UInt3            = reg1 & 0x07
  def lengthCounterLoad: UInt5     = (reg3 >> 3) & 0x1f
  def timerPeriod: UInt11          = ((reg3 & 0x07) << 8) | reg2
}

object PulseState {
  val reg0: Setter[PulseState, UInt8]                 = (a, s) => s.reg0 = a
  val reg1: Setter[PulseState, UInt8]                 = (a, s) => s.reg1 = a
  val reg2: Setter[PulseState, UInt8]                 = (a, s) => s.reg2 = a
  val reg3: Setter[PulseState, UInt8]                 = (a, s) => s.reg3 = a
  val lengthCounterEnabled: Setter[PulseState, UInt1] = (a, s) => s.lengthCounterEnabled = a
  val lengthCounterValue: Setter[PulseState, UInt4]   = (a, s) => s.lengthCounterValue = a
  val timerValue: Setter[PulseState, UInt11]          = (a, s) => s.timerValue = a

  type Channel1 = PulseState with OnesComplementAdder
  type Channel2 = PulseState with TwosComplementAdder

  sealed trait Adder
  trait OnesComplementAdder extends Adder
  trait TwosComplementAdder extends Adder

  def channel1(): Channel1 =
    new PulseState(
      0x00, 0x00, 0x00, 0x00, 0, 0, 0, 0, 0
    ) with OnesComplementAdder

  def channel2(): Channel2 =
    new PulseState(
      0x00, 0x00, 0x00, 0x00, 0, 0, 0, 0, 0
    ) with TwosComplementAdder
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
  def setPulse1(pulse: PulseState.Channel1)(nes: NesState): NesState = {
    val apu  = ApuState.pulse1.set(pulse)(nes.apuState)
    val nes1 = NesState.apuState.set(apu)(nes)
    nes1
  }

  def setPulse2(pulse: PulseState.Channel2)(nes: NesState): NesState = {
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
          setPulse1(PulseState.reg2.set(d)(nes.apuState.pulse1))(nes)
        case 0x4003 =>
          setPulse1(PulseState.reg3.set(d)(nes.apuState.pulse1))(nes)
        case 0x4004 =>
          setPulse2(PulseState.reg0.set(d)(nes.apuState.pulse2))(nes)
        case 0x4005 =>
          setPulse2(PulseState.reg1.set(d)(nes.apuState.pulse2))(nes)
        case 0x4006 =>
          setPulse2(PulseState.reg2.set(d)(nes.apuState.pulse2))(nes)
        case 0x4007 =>
          setPulse2(PulseState.reg3.set(d)(nes.apuState.pulse2))(nes)
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
          nes
        case 0x4017 =>
          nes
        case _ => throw new RuntimeException(f"Invalid cpu memory write at address $address%#04x")
      }
    }
}
