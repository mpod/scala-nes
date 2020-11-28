package scalanes.mutable

class ControllerState(
  var buttons: UInt8,
  var controller1: UInt8,
  var controller2: UInt8
)

object ControllerState {
  val buttons: Setter[ControllerState, UInt8]     = (a, s) => s.buttons = a
  val controller1: Setter[ControllerState, UInt8] = (a, s) => s.controller1 = a
  val controller2: Setter[ControllerState, UInt8] = (a, s) => s.controller2 = a

  def apply(): ControllerState =
    new ControllerState(0x00, 0x00, 0x00)
}

object Controller {

  val serialReadController1: State[NesState, UInt8] =
    nes => {
      val c    = nes.controllerState
      val bit  = (c.controller1 & 0x80) >> 7
      val ctrl = ControllerState.controller1.set((c.controller1 << 1) & 0xff)(c)
      val nes1 = NesState.controllerState.set(ctrl)(nes)
      (nes1, bit)
    }

  val serialReadController2: State[NesState, UInt8] =
    nes => {
      val c    = nes.controllerState
      val bit  = (c.controller2 & 0x80) >> 7
      val ctrl = ControllerState.controller2.set((c.controller2 << 1) & 0xff)(c)
      val nes1 = NesState.controllerState.set(ctrl)(nes)
      (nes1, bit)
    }

  val writeController1: State[NesState, Unit] =
    nes => {
      val ctrl  = nes.controllerState
      val ctrl1 = ControllerState.controller1.set(ctrl.buttons)(ctrl)
      val nes1  = NesState.controllerState.set(ctrl1)(nes)
      (nes1, ())
    }

  val writeController2: State[NesState, Unit] =
    nes => {
      val ctrl  = nes.controllerState
      val ctrl1 = ControllerState.controller2.set(ctrl.buttons)(ctrl)
      val nes1  = NesState.controllerState.set(ctrl1)(nes)
      (nes1, ())
    }

}
