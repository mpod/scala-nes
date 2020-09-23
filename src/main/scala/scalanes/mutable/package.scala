package scalanes

import cats.{Monad, Monoid}
import cats.data.{IndexedStateT, StateT}
import cats.effect.IO
import cats.effect.concurrent.Ref
import monocle.Lens

import scala.annotation.tailrec
import scala.language.implicitConversions

package object mutable {

  type UInt1 = Int
  type UInt2 = Int
  type UInt3 = Int
  type UInt5 = Int
  type UInt8 = Int
  type UInt15 = Int
  type UInt16 = Int

  type ControllerRef = Ref[IO, UInt8]

  def isValidUInt1(v: UInt1): Boolean = (v & 0x01) == v
  def isValidUInt2(v: UInt2): Boolean = (v & 0x03) == v
  def isValidUInt3(v: UInt3): Boolean = (v & 0x07) == v
  def isValidUInt5(v: UInt5): Boolean = (v & 0x1F) == v
  def isValidUInt8(v: UInt8): Boolean = (v & 0xFF) == v
  def isValidUInt15(v: UInt15): Boolean = (v & 0x7FFF) == v
  def isValidUInt16(v: UInt16): Boolean = (v & 0xFFFF) == v

  implicit def intToBoolean(v: Int): Boolean = v != 0

  case class State[S, A](run: S => (S, A)) {
    def map[B](f: A => B): State[S, B] =
      flatMap(a => State.pure(f(a)))

    def transformS[R](f: R => S, g: (R, S) => R): State[R, A] = State { r =>
      val (s, a) = run(f(r))
      (g(r, s), a)
    }

    def transform[B](f: (S, A) => (S, B)): State[S, B] = State { s =>
      val (s1, a) = run(s)
      f(s1, a)
    }

    def modify(f: S => S): State[S, A] = transform((s, a) => (f(s), a))

    def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
      val (s1, a) = run(s)
      f(a).run(s1)
    })

    def >>[B](fb: => State[S, B]): State[S, B] = flatMap(_ => fb)

    def *>[B](fb: State[S, B]): State[S, B] = >>(fb)

    def runS(s: S): S = run(s)._1
  }

  object State {
    def pure[S, A](a: A): State[S, A] = State(s => (s, a))
    def empty[S, A](implicit A: Monoid[A]): State[S, A] = pure(A.empty)
    def modify[S](f: S => S): State[S, Unit] = State(s => (f(s), ()))
    def inspect[S, T](f: S => T): State[S, T] = State(s => (s, f(s)))
    def get[S]: State[S, S] = State(s => (s, s))
    def set[S](s: S): State[S, Unit] = State(_ => (s, ()))

    implicit val stateMonad: Monad[State[NesState, *]] = new Monad[State[NesState, *]] {
      def flatMap[A, B](fa: State[NesState, A])(f: A => State[NesState, B]): State[NesState, B] = fa.flatMap(f)
      def pure[A](a: A): State[NesState, A] = State.pure(a)

      def tailRecM[A, B](a: A)(f: A => State[NesState, Either[A, B]]): State[NesState, B] =
        flatMap(f(a)) {
          case Right(b) => pure(b)
          case Left(nextA) => tailRecM(nextA)(f)
        }
    }
  }

  def lens[S, A](getter: S => A, setter: S => A => Unit): Lens[S, A] =
    Lens.apply(getter) { a: A => s: S =>
      setter(s)(a)
      s
    }

  def hex(n: Int, d: Int): String = (d - 1 to 0 by -1).map(i => "0123456789ABCDEF"((n >> (i * 4)) & 0xF)).mkString("")
}
