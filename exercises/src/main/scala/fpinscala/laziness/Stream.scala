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

  def drop(n: Int): Stream[A] = {
    def go(str: Stream[A], i: Int): Stream[A] = {
      str match {
        case Empty => Empty
        case Cons(h, t) if (n == i) => cons[A](h(), t())
        case Cons(h, t) => go(t(), i+1)
      }
    }
    go(this, 0)
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
      def go(str: Stream[A], acc: Stream[A]): Stream[A] = {
        str match {
          case Cons(h, t) if p(h()) => go(t(), cons(h(), acc))
          case _ => acc
        }
      }
      go(this, Empty).reverse()
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = sys.error("todo")

  def forAll(p: A => Boolean): Boolean = {
    def go(str: Stream[A]): Boolean = {
      str match {
        case Cons(h, t) => p(h()) && go(t())
        case _ => true
      }
    }
    go(this)
  }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = {
    foldRight(Empty: Stream[A])((h, acc) => if (!p(h)) Empty else cons(h, acc))
  }

  def headOption: Option[A] = {
    this match {
      case Empty => None
      case Cons(h,t) => Some(h())
    }
  }

  def headOptionViaFoldRight: Option[A] = {
    this.foldRight(None:Option[A])((h,acc) => Some(h))
  }

  def map[B](f: A => B): Stream[B] = {
    def go(str: Stream[A]): Stream[B] = {
      str match {
        case Empty => Empty
        case Cons(h, t) => cons(f(h()), go(t()))
      }
    }
    go(this)
  }

  def mapViaUnfold[B](f: A => B): Stream[B] = sys.error("todo")

  def filter(p: A => Boolean): Stream[A] = {
    this.foldRight(Empty:Stream[A])((h, acc) => if (p(h)) cons[A](h, acc) else acc)
  }

  def append[B>:A](other: Stream[B]): Stream[B] = {
    this.foldRight(other)((h,acc) => cons(h, acc))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    this.foldRight(Empty:Stream[B])((h, acc) => f(h).append(acc))
  }

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] = sys.error("todo")

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = sys.error("todo")

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")

  def tails: Stream[Stream[A]] = sys.error("todo using unfold")

  def scanRight[B](s: B)(f: (A, B) => B): Stream[B] = sys.error("todo")
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

  def constant[A](a: A): Stream[A] = {
    Stream.cons(a, constant(a))
  }

  def from(n: Int): Stream[Int] = {
    Stream.cons(n, from(n + 1))
  }

  lazy val fibs: Stream[Int] = sys.error("todo")

  val fibs: Stream[Int] = {
    def go(current: Int, next: Int): Stream[Int] = {
      Stream.cons(current, go(next, next + current))
    }
    go(0, 1)
  }

  lazy val fibsViaUnfold: Stream[Int] = sys.error("todo")

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    def go(acc: Stream[A], zz: S):Stream[A] = {
      f(zz) match {
        case None => acc
        case Some(t:(A,S)) => go(Stream.cons(t._1, acc), t._2)
      }
    }
    go(Empty,z).reverse()
  }

  val fibsViaUnfold: Stream[Int] = {
    var limit = 100
    unfold[Int, (Int,Int)]((0,1))((i:(Int,Int)) => if (i._1 > limit) None
                                                else Some((i._1,(i._2, i._1 + i._2))))
  }

  def fromViaUnfold(n: Int): Stream[Int] = {
    var limit = 100
    unfold(n)(nn => if (nn > limit) None else Some((nn, nn + 1)))
  }


  def constantViaUnfold[A](a: A): Stream[A] = {
    var limit = 1000
    unfold(a)(a => if (limit == 0) None else {limit = limit - 1; Some(a,a)})
  }

  val onesViaUnfold: Stream[Int] = constantViaUnfold(1)

  lazy val onesViaUnfold: Stream[Int] = sys.error("todo")
}