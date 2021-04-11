import scala.language.implicitConversions

package object scalanes {

  type UInt1  = Int
  type UInt2  = Int
  type UInt3  = Int
  type UInt4  = Int
  type UInt5  = Int
  type UInt7  = Int
  type UInt8  = Int
  type UInt11 = Int
  type UInt12 = Int
  type UInt15 = Int
  type UInt16 = Int

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
  }

  abstract class Setter[S, A] {
    protected def _set(a: A, s: S): Unit
    def set[T <: S](a: A)(s: T): T = {
      _set(a, s)
      s
    }
  }

  abstract class IndexSetter[S, A] {
    protected def _set(i: Int, a: A, s: S): Unit
    def set(i: Int, a: A)(s: S): S = {
      _set(i, a, s)
      s
    }
  }

  def hex(n: Int, d: Int = 4): String =
    (d - 1 to 0 by -1).map(i => "0123456789ABCDEF" ((n >> (i * 4)) & 0xf)).mkString("")
}
