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

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, nr) = rng.nextInt
    (math.abs(i), nr)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, nr) = nonNegativeInt(rng)
    (i.toDouble / Int.MaxValue, nr)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, nr) = nonNegativeInt(rng)
    val (d, nr2) = double(nr)
    ((i, d), nr2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, nr) = double(rng)
    val (i, nr2) = nonNegativeInt(nr)
    ((d, i), nr2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r) = double(rng)
    val (d2, r2) = double(r)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val (l, r) = List.iterate(rng.nextInt, count)(_._2.nextInt).unzip
    (l, r.last)
  }

  val doubleMap: (RNG) => (Double, RNG) = map(nonNegativeInt)(_.toDouble / Int.MaxValue)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = { rng =>
    val (ea, nra) = ra(rng)
    val (eb, nrb) = rb(nra)
    (f(ea, eb), nrb)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight[Rand[List[A]]](unit(Nil)) { (e, r) =>
    map2(e, r)(_ :: _)
  }

  def intsSequence(count: Int)(rng: RNG): (List[Int], RNG) = sequence(List.fill(count)((_: RNG).nextInt))(rng)

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = (rng) => {
    val (a, na) = f(rng)
    g(a)(na)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
    }

  def mapFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def map2FlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra) { a =>
    map(rb)(b => f(a, b))
  }
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State { s =>
    val (a, b) = run(s)
    (f(a), b)
  }

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
    val (a, b) = run(s)
    f(a).run(b)
  }

}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {

  def unit[S, T](t: T) = State[S, T](s => (t, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
    fs.foldRight(unit[S, List[A]](Nil)) { (f, r) =>
      f.map2(r)(_ :: _)
    }
  }

  type Rand[A] = State[RNG, A]

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State { ma =>
    inputs match {
      case h :: t =>
        val nma = if (ma.locked && h == Coin) {
            ma.copy(locked = false, coins = ma.coins + 1)
        } else if (h == Turn)
            ma.copy(locked = true, candies = ma.candies - 1)
          else ma
        simulateMachine(t).run(nma)
      case Nil =>
        ((ma.coins, ma.candies), ma)
    }
  }

  def main(args: Array[String]): Unit = {
    val ma = Machine(true, 5, 10)
    simulateMachine(List.fill(4)(List(Coin, Turn)).flatten).run(ma) match {
      case ((coins, candies), nma) =>
        assert(candies == 1)
        assert(coins == 14)
    }

  }
}
