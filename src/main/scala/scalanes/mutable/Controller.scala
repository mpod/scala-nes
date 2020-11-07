package scalanes.mutable

class ControllerState(var ref: ControllerRef) {
  var controller1: UInt8 = 0x00
  var controller2: UInt8 = 0x00
}

object ControllerState {
  val ref: Setter[ControllerState, ControllerRef] = (a, s) => s.ref = a
  val controller1: Setter[ControllerState, UInt8] = (a, s) => s.controller1 = a
  val controller2: Setter[ControllerState, UInt8] = (a, s) => s.controller2 = a
}

object Controller {

  def serialReadController1: State[NesState, UInt8] = State { nes =>
    val c    = nes.controllerState
    val bit  = (c.controller1 & 0x80) >> 7
    val c1   = ControllerState.controller1.set((nes.controllerState.controller1 << 1) & 0xff)(c)
    val nes1 = NesState.controllerState.set(c1)(nes)
    (nes1, bit)
  }

  def serialReadController2: State[NesState, UInt8] = State { nes =>
    val c    = nes.controllerState
    val bit  = (c.controller2 & 0x80) >> 7
    val c1   = ControllerState.controller2.set((nes.controllerState.controller2 << 1) & 0xff)(c)
    val nes1 = NesState.controllerState.set(c1)(nes)
    (nes1, bit)
  }

  def writeController1: State[NesState, Unit] = State { nes =>
    nes.controllerState.ref
      .getAndSet(0x00)
      .map { a =>
        val c    = nes.controllerState
        val c1   = ControllerState.controller1.set(a)(c)
        val nes1 = NesState.controllerState.set(c1)(nes)
        (nes1, ())
      }
      .unsafeRunSync()
  }

  def writeController2: State[NesState, Unit] = State { nes =>
    nes.controllerState.ref
      .getAndSet(0x00)
      .map { a =>
        val c    = nes.controllerState
        val c1   = ControllerState.controller2.set(a)(c)
        val nes1 = NesState.controllerState.set(c1)(nes)
        (nes1, ())
      }
      .unsafeRunSync()
  }

}
