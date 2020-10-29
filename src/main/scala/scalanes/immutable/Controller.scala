package scalanes.immutable

import cats.data.StateT
import monocle.Lens
import monocle.macros.GenLens

case class ControllerState(ref: ControllerRef, controller1: UInt8, controller2: UInt8)

object ControllerState {
  val ref: Lens[ControllerState, ControllerRef] = GenLens[ControllerState](_.ref)
  val controller1: Lens[ControllerState, UInt8] = GenLens[ControllerState](_.controller1)
  val controller2: Lens[ControllerState, UInt8] = GenLens[ControllerState](_.controller2)

  def apply(ref: ControllerRef): ControllerState = ControllerState(ref, 0x00, 0x00)
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

  def writeController1: State[NesState, Unit] = StateT { ns =>
    ns.controllerState.ref.getAndSet(0x00).map { a =>
      val update = (NesState.controllerState composeLens ControllerState.controller1).set(a)
      (update(ns), ())
    }
  }

  def writeController2: State[NesState, Unit] = StateT { ns =>
    ns.controllerState.ref.getAndSet(0x00).map { a =>
      val update = (NesState.controllerState composeLens ControllerState.controller2).set(a)
      (update(ns), ())
    }
  }

}
