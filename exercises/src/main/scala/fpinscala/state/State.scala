package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
      val (i, rng1) = rng.nextInt
      val ii = if (i >= 0) i else ((i+1) * -1)
      (ii, rng1)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, rng1) = nonNegativeInt(rng)
    ((i.toDouble)/ (Int.MaxValue.toDouble + 1), rng1)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng1) = nonNegativeInt(rng)
    val (dbl, rng2) = double(rng1)
    ((i,dbl), rng2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (intDbl, rng1) = intDouble(rng)
    ((intDbl._2,intDbl._1), rng1)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1,d2,d3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(rng: RNG, countdown: Int): (List[Int], RNG) = {
      if (countdown == 0) {
        (List(), rng)
      } else {
        val (i, rngNext) = rng.nextInt
        if (countdown == 1) {
          (i::Nil, rngNext)
        } else {
          val (li, rngFinal) = go(rngNext, countdown - 1)
          (i::li, rngFinal)
        }
      }
    }
    go(rng, count)
  }

  def doubleViaMap: Rand[Double] = ???

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???

  def nonNegativeLessThan(n: Int): Rand[Int] = ???

  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = ???

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    sys.error("todo")
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = ???

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = ???

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}