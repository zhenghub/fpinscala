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
    val (i, nr) = rng.nextInt
    (math.abs(i), nr)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, nr) = nonNegativeInt(rng)
    (i.toDouble / Int.MaxValue, nr)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, nr) = nonNegativeInt(rng)
    val (d, nr2) = double(nr)
    ((i, d), nr2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d, nr) = double(rng)
    val (i, nr2) = nonNegativeInt(nr)
    ((d, i), nr2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r) = double(rng)
    val (d2, r2) = double(r)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val (l ,r) = List.iterate(rng.nextInt, count)(_._2.nextInt).unzip
    (l, r.last)
  }

  val doubleMap: (RNG) => (Double, RNG) = map(nonNegativeInt)(_.toDouble / Int.MaxValue)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = { rng =>
    val (ea, nra) = ra(rng)
    val (eb, nrb) = rb(nra)
    (f(ea, eb), nrb)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight[Rand[List[A]]](unit(Nil)) { (e, r) =>
    map2(e, r)(_ :: _)
  }

  def intsSequence(count: Int)(rng: RNG): (List[Int], RNG) = sequence(List.fill(count)((_: RNG).nextInt))(rng)

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
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
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
