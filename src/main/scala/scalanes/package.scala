import cats.Monoid
import cats.data.{IndexedStateT, StateT}
import cats.effect.IO
import eu.timepit.refined.W
import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.Size
import eu.timepit.refined.generic.Equal

import scala.language.implicitConversions

package object scalanes {

  type UInt1 = Int
  type UInt2 = Int
  type UInt3 = Int
  type UInt5 = Int
  type UInt8 = Int
  type UInt15 = Int
  type UInt16 = Int

  type Cartridge = Mapper

  // TODO: Explore later...
  type RAM = Vector[UInt8] Refined Size[Equal[W.`65536`.T]]

  implicit def intToBoolean(value: Int): Boolean = value != 0

  def hex(n: Int, d: Int): String = (d - 1 to 0 by -1).map(i => "0123456789ABCDEF"((n >> (i * 4)) & 0xF)).mkString("")

  type State[S, A] = StateT[IO, S, A]

  object State {
    def apply[S, A](f: S => (S, A)): State[S, A] = IndexedStateT.applyF(IO.pure((s: S) => IO.pure(f(s))))
    def pure[S, A](a: A): State[S, A] = State(s => (s, a))
    def empty[S, A](implicit A: Monoid[A]): State[S, A] = pure(A.empty)
    def modify[S](f: S => S): State[S, Unit] = State(s => (f(s), ()))
    def inspect[S, T](f: S => T): State[S, T] = State(s => (s, f(s)))
    def get[S]: State[S, S] = inspect(identity)
    def set[S](s: S): State[S, Unit] = State(_ => (s, ()))
  }
}
