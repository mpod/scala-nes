package scalanes.mutable

import monocle.Lens

class ControllerState(var ref: ControllerRef) {
  var controller1: UInt8 = 0x00
  var controller2: UInt8 = 0x00
}

object ControllerState {
  val ref: Lens[ControllerState, ControllerRef] = lens[ControllerState, ControllerRef](_.ref, _.ref_=)
  val controller1: Lens[ControllerState, UInt8] = lens[ControllerState, UInt8](_.controller1, _.controller1_=)
  val controller2: Lens[ControllerState, UInt8] = lens[ControllerState, UInt8](_.controller2, _.controller2_=)
}

object Controller {

  def serialReadController1: State[NesState, UInt8] = State { ns =>
    val c      = ns.controllerState
    val bit    = (c.controller1 & 0x80) >> 7
    val update = (NesState.controllerState composeLens ControllerState.controller1).modify(a => (a << 1) & 0xff)
    (update(ns), bit)
  }

  def serialReadController2: State[NesState, UInt8] = State { ns =>
    val c      = ns.controllerState
    val bit    = (c.controller2 & 0x80) >> 7
    val update = (NesState.controllerState composeLens ControllerState.controller2).modify(a => (a << 1) & 0xff)
    (update(ns), bit)
  }

  def writeController1: State[NesState, Unit] = State { ns =>
    ns.controllerState.ref
      .getAndSet(0x00)
      .map { a =>
        val update = (NesState.controllerState composeLens ControllerState.controller1).set(a)
        (update(ns), ())
      }
      .unsafeRunSync()
  }

  def writeController2: State[NesState, Unit] = State { ns =>
    ns.controllerState.ref
      .getAndSet(0x00)
      .map { a =>
        val update = (NesState.controllerState composeLens ControllerState.controller2).set(a)
        (update(ns), ())
      }
      .unsafeRunSync()
  }

}
