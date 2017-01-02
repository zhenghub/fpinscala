package fpinscala.laziness

import Stream._

import scala.annotation.tailrec

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = n match {
    case 0 => Empty
    case _ =>
      this match {
        case Empty => Empty
        case Cons(h, t) =>
          Cons(h, () => t().take(n - 1))
      }
  }

  def drop(n: Int): Stream[A] = n match {
    case 0 => this
    case _ =>
      this match {
        case Empty => Empty
        case Cons(h, t) =>
          t().drop(n - 1)
      }
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) =>
      val v = h()
      if (p(v)) {
        Cons(() => v, () => t().takeWhile(p))
      } else {
        Empty
      }
  }

  def takeWhileFoldRight(p: A => Boolean): Stream[A] = foldRight[Stream[A]](Empty) { (e, s) =>
    if (p(e)) {
      cons(e, s)
    } else {
      Empty
    }
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true) { (e, b) =>
    p(e) && b
  }

  def headOption: Option[A] = foldRight[Option[A]](None) { (e, o) =>
    Some(e)
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = foldRight[Stream[B]](Empty) { (e, s) =>
    cons(f(e), s)
  }

  def filter(p: A => Boolean) = foldRight[Stream[A]](Empty) { (e, s) =>
    if (p(e)) {
      cons(e, s)
    } else s
  }

  def append[E >: A](a: => E) = foldRight[Stream[E]](cons(a, Empty)) { (e, s) =>
    cons(e, s)
  }

  def flatMap[B](f: A => Stream[B]) = foldRight[Stream[B]](Empty) { (e, s) =>
    f(e).foldRight(s)(cons(_, _))
  }

  def mapUnfold[B](f: A => B): Stream[B] = Stream.unfold(() => this) { s =>
    val Cons(fe, t) = s()
    Some(f(fe()) -> t)
  }

  def takeUnfold(n: Int): Stream[A] = Stream.unfold(0 -> (() => this)) { case (c, s) =>
    if (c < n) {
      val Cons(fe, t) = s()
      Some(fe() ->(c + 1, t))
    } else None
  }

  def takeWhileUnfold(p: A => Boolean): Stream[A] = Stream.unfold(() => this) { s =>
    val Cons(fe, t) = s()
    val e = fe()
    if (p(e))
      Some(e, t)
    else
      None
  }

  def zipWithUnfold[B, C](os: Stream[B])(f: (A, B) => C): Stream[C] = Stream.unfold((() => this) -> (() => os)) { case (ft, fo) =>
    val (t, o) = ft() -> fo()
    if (t != Empty && o != Empty) {
      val Cons(tf, tt) = t
      val Cons(of, ot) = o
      Some((f(tf(), of())) ->(tt, ot))
    } else None
  }

  def zipAllUnfold[B](os: Stream[B]): Stream[(Option[A], Option[B])] = {
    def ex[E](f: () => Stream[E]) = f() match {
      case Empty =>
        (None, f)
      case Cons(f, t) =>
        (Some(f()), t)
    }
    Stream.unfold((() => this) -> (() => os)) { case (ft, fo) =>
      val (e1, s1) = ex(ft)
      val (e2, s2) = ex(fo)
      if (e1.isDefined || e2.isDefined) {
        Some((e1, e2) ->(s1, s2))
      } else None
    }
  }

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
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

  def ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = {
    lazy val c: Stream[A] = cons(a, c)
    c
  }

  def from(n: Int): Stream[Int] = {
    lazy val ns: Stream[Int] = cons(n, ns.map(_ + 1))
    ns
  }

  def fibs: Stream[Int] = {
    def zip(f: Stream[Int], s: Stream[Int]): Stream[Int] = {
      val Cons(fh, ft) = f
      val Cons(sh, st) = s
      Cons(() => fh() + sh(), () => zip(ft(), st()))
    }

    lazy val r: Stream[Int] = cons(0, cons(1, cons(1, {
      zip(r.drop(1), r.drop(2))
    })))
    r
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => Empty
    case Some((a, s)) =>
      cons(a, unfold(s)(f))
  }

  def fibsUnfold: Stream[Int] = unfold((0, 1)) { case (f, s) =>
    Some(f ->(s, f + s))
  }

  def fromUnfold(n: Int) = unfold(n)(n => Some(n, n + 1))

  def constantUnfold[A](a: A) = unfold(a)(a => Some(a, a))

  def onesUnfold = unfold(1)(_ => Some(1, 1))
}

object TestSteam {
  def main(args: Array[String]): Unit = {
    assert(Stream(1, 2, 3).takeWhile(_ < 3).toList == List(1, 2))

    assert(Stream(1, 2, 3).headOption == Some(1))
    assert(Empty.headOption == None)

    Stream(1, 2).append {
      sys.error("不应该执行到这")
      3
    }

    assert(Stream(1, 2, 3).drop(1).toList == List(2, 3))
    assert(Stream.from(2).take(3).toList == List(2, 3, 4))
    assert(Stream(1, 2, 3).map(_ * 2).toList == List(2, 4, 6))
    assert(Stream.fibs.map { x =>
      println(x)
      x
    }.take(5).toList == List(0, 1, 1, 2, 3))
    assert(Stream.fibs.take(5).toList == Stream.fibsUnfold.take(5).toList)
    assert(Stream.fromUnfold(2).take(3).toList == List(2, 3, 4))
  }
}