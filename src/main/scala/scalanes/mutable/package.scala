package scalanes

import cats.Monad
import cats.effect.IO
import cats.effect.concurrent.Ref
import monocle.Lens

import scala.annotation.tailrec
import scala.language.implicitConversions

package object mutable {

  type UInt1  = Int
  type UInt2  = Int
  type UInt3  = Int
  type UInt5  = Int
  type UInt8  = Int
  type UInt15 = Int
  type UInt16 = Int

  type ControllerRef = Ref[IO, UInt8]

  def isValidUInt1(v: UInt1): Boolean   = (v & 0x01) == v
  def isValidUInt2(v: UInt2): Boolean   = (v & 0x03) == v
  def isValidUInt3(v: UInt3): Boolean   = (v & 0x07) == v
  def isValidUInt5(v: UInt5): Boolean   = (v & 0x1f) == v
  def isValidUInt8(v: UInt8): Boolean   = (v & 0xff) == v
  def isValidUInt15(v: UInt15): Boolean = (v & 0x7fff) == v
  def isValidUInt16(v: UInt16): Boolean = (v & 0xffff) == v

  implicit def intToBoolean(v: Int): Boolean = v != 0

  type State[S, A] = S => (S, A)

  implicit class StateOps[S, A](val run: State[S, A]) extends AnyVal {
    def map[B](f: A => B): State[S, B] =
      flatMap(a => State.pure(f(a)))

    def transformS[R](f: R => S, g: (R, S) => R): State[R, A] =
      r => {
        val (s, a) = run(f(r))
        (g(r, s), a)
      }

    def transform[B](f: (S, A) => (S, B)): State[S, B] =
      s => {
        val (s1, a) = run(s)
        f(s1, a)
      }

    def modify(f: S => S): State[S, A] = transform((s, a) => (f(s), a))

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      s => {
        val (s1, a) = run(s)
        f(a)(s1)
      }

    def >>[B](fb: => State[S, B]): State[S, B] = flatMap(_ => fb)

    def *>[B](fb: State[S, B]): State[S, B] = >>(fb)

    def runS(s: S): S = run(s)._1

    def runA(s: S): A = run(s)._2
  }

  object State {
    def apply[S, A](sf: S => (S, A)): State[S, A] = sf
    def pure[S, A](a: A): State[S, A]             = s => (s, a)
    def modify[S](f: S => S): State[S, Unit]      = s => (f(s), ())
    def inspect[S, T](f: S => T): State[S, T]     = s => (s, f(s))
    def get[S]: State[S, S]                       = s => (s, s)
    def set[S](s: S): State[S, Unit]              = _ => (s, ())

    implicit val stateMonad: Monad[State[NesState, *]] = new Monad[State[NesState, *]] {
      def flatMap[A, B](fa: State[NesState, A])(f: A => State[NesState, B]): State[NesState, B] =
        s => {
          val (nextS, a) = fa(s)
          f(a)(nextS)
        }

      def pure[A](a: A): State[NesState, A] = State.pure(a)

      def tailRecM[A, B](a: A)(f: A => State[NesState, Either[A, B]]): State[NesState, B] =
        (s: NesState) => {
          @tailrec
          def step(thisA: A): (NesState, B) = f(thisA)(s) match {
            case (nextS, Right(b)) => (nextS, b)
            case (_, Left(nextA))  => step(nextA)
          }
          step(a)
        }
    }
  }

  def lens[S, A](getter: S => A, setter: S => A => Unit): Lens[S, A] =
    Lens.apply(getter) { a: A => s: S =>
      setter(s)(a)
      s
    }

  case class MLens[S, A](getter: S => A, setter: (S, A) => Unit) {
    def get(s: S): A = getter(s)
    def set(a: A, s: S): S = {
      setter(s, a)
      s
    }
  }

  def hex(n: Int, d: Int = 4): String =
    (d - 1 to 0 by -1).map(i => "0123456789ABCDEF" ((n >> (i * 4)) & 0xf)).mkString("")
}
