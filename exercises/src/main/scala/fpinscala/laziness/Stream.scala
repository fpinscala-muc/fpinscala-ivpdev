package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = {
    def go(str: Stream[A]): List[A] = {
      str match {
        case Empty => List():List[A]
        case Cons(h, t) => h() :: go(t())
      }
    }
    go(this)
  }

  //not for infinite
  def reverse() = {
    def go(str: Stream[A], acc: Stream[A]): Stream[A] = {
      str match {
        case Empty => acc
        case Cons(h, t) => go(t(), Stream.cons(h(), acc))
      }
    }
    go(this, Empty)
  }

  def printStr() = {
    def go(str: Stream[A]): Any = {
      str match {
        case Empty => Nil
        case Cons(h, t) => {
          print(h() + " ")
          go(t())
        }
      }
    }
    print("[")
    go(this)
    print("]")
  }

  //not for infinite
  def take(n: Int): Stream[A] = {
    def go(str: Stream[A], acc: Stream[A], i: Int): Stream[A] = {
      str match {
        case Empty => acc
        case _ if (i == n) => acc
        case Cons(h, t) => go(t(), cons(h(), acc), i+1)

      }
    }
    go(this, Empty, 0).reverse()
  }

  def takeViaUnfold(n: Int): Stream[A] = sys.error("todo")

  def drop(n: Int): Stream[A] = sys.error("todo")

  def takeWhile(p: A => Boolean): Stream[A] = sys.error("todo")

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = sys.error("todo")

  def forAll(p: A => Boolean): Boolean = sys.error("todo")

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = sys.error("todo")

  def headOption: Option[A] = sys.error("todo")

  def map[B](f: A => B): Stream[B] = sys.error("todo")

  def mapViaUnfold[B](f: A => B): Stream[B] = sys.error("todo")

  def filter(p: A => Boolean): Stream[A] = sys.error("todo")

  def append[B>:A](other: Stream[B]): Stream[B] = sys.error("todo")

  def flatMap[B](f: A => Stream[B]): Stream[B] = sys.error("todo")

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] = sys.error("todo")

  def startsWith[A](s: Stream[A]): Boolean = sys.error("todo")
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = sys.error("todo")

  def from(n: Int): Stream[Int] = sys.error("todo")

  //TODO stub for unit test
  val fibs: Stream[Int] = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0+f1))
    go(0, 1)
  }


  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")

  //TODO stub for unit test
  val fibsViaUnfold: Stream[Int] = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0+f1))
    go(0, 1)
  }

  def fromViaUnfold(n: Int): Stream[Int] = sys.error("todo")

  def constantViaUnfold[A](a: A): Stream[A] = sys.error("todo")

  //TODO stub for unit test
  val onesViaUnfold: Stream[Int] = Stream.cons(1, ones)

}