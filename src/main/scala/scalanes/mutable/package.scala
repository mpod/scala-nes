package scalanes

import cats.Monoid
import cats.data.{IndexedStateT, StateT}
import cats.effect.IO
import monocle.Lens
import scalanes.mutable.mappers.Mapper000

import scala.language.implicitConversions

package object mutable {

  type UInt1 = Int
  type UInt2 = Int
  type UInt3 = Int
  type UInt5 = Int
  type UInt8 = Int
  type UInt15 = Int
  type UInt16 = Int

  def isValidUInt1(v: UInt1): Boolean = (v & 0x01) == v
  def isValidUInt2(v: UInt2): Boolean = (v & 0x03) == v
  def isValidUInt3(v: UInt3): Boolean = (v & 0x07) == v
  def isValidUInt5(v: UInt5): Boolean = (v & 0x1F) == v
  def isValidUInt8(v: UInt8): Boolean = (v & 0xFF) == v
  def isValidUInt15(v: UInt15): Boolean = (v & 0x7FFF) == v
  def isValidUInt16(v: UInt16): Boolean = (v & 0xFFFF) == v

  implicit def intToBoolean(v: Int): Boolean = v != 0

  type State[S, A] = StateT[IO, S, A]

  object State {
    def apply[S, A](f: S => (S, A)): State[S, A] = IndexedStateT.applyF(IO.pure((s: S) => IO.pure(f(s))))
    def pure[S, A](a: A): State[S, A] = State(s => (s, a))
    def empty[S, A](implicit A: Monoid[A]): State[S, A] = pure(A.empty)
    def modify[S](f: S => S): State[S, Unit] = State(s => (f(s), ()))
    def inspect[S, T](f: S => T): State[S, T] = State(s => (s, f(s)))
    def get[S]: State[S, S] = State(s => (s, s))
    def set[S](s: S): State[S, Unit] = State(_ => (s, ()))
  }

  def lens[S, A](getter: S => A, setter: S => A => Unit): Lens[S, A] =
    Lens.apply(getter) { a: A => s: S =>
      setter(s)(a)
      s
    }

  def hex(n: Int, d: Int): String = (d - 1 to 0 by -1).map(i => "0123456789ABCDEF"((n >> (i * 4)) & 0xF)).mkString("")
}
