package fpinscala.datastructures

import scala.annotation.tailrec

/**
  * Created by zh on 16-12-9.
  */
object E3_1 {
  def main(args: Array[String]): Unit = {
    assert(3 == List.x)
  }
}

object E3_8 {
  def main(args: Array[String]): Unit = {
    assert(List(1, 2, 3, 4) == List.foldRight(List(1, 2, 3, 4), Nil: List[Int])(Cons(_, _)))
  }
}

object E3_11 {

  def sum(l: List[Int]) = List.foldLeft(l, 0)(_ + _)

  def product(l: List[Double]) = List.foldLeft(l, 1.0)(_ * _)

  def length[A](l: List[A]) = List.foldLeft(l, 0)((c, _) => c + 1)

}

object E3_12 {

  def reverse[A](l: List[A]) = List.foldLeft(l, Nil: List[A])((c, e) => Cons(e, c))

  def main(args: Array[String]): Unit = {
    assert(List(3, 2, 1) == reverse(List(1, 2, 3)))
  }

}

object E3_13 {

  def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    List.foldRight(E3_12.reverse(l), z)((a, b) => f(b, a))
  }

  def foldRight2[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    List.foldLeft(E3_12.reverse(l), z)((b, a) => f(a, b))
  }

}

object E3_14 {
  def append[A](l: List[A], e: A): List[A] = {
    List.foldRight(l, List(e))(Cons(_, _))
  }
}

object E3_15 {
  def concat[A](lists: List[A]*): List[A] = {
    List.foldRight(List(lists: _*), Nil: List[A])((l, r) => List.foldRight(l, r)(Cons(_, _)))
  }
}

object E3_16 {
  def incr(list: List[Int]) = {
    List.foldRight(list, Nil: List[Int])((i, r) => Cons(i + 1, r))
  }
}

object E3_17 {
  def toString(list: List[Double]) = {
    List.foldRight(list, Nil: List[String])((d, r) => Cons(d.toString, r))
  }
}

object E3 {
  def map[A, B](as: List[A])(f: A => B) = {
    List.foldRight(as, Nil: List[B])((e, r) => Cons(f(e), r))
  }

  def filter[A](as: List[A])(f: A => Boolean) = {
    List.foldRight(as, Nil: List[A])((e, r) => if (f(e)) Cons(e, r) else r)
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    List.foldLeft(as, Nil: List[B])((l, e) => E3_15.concat(l, f(e)))
  }

  def filter2[A](as: List[A])(f: A => Boolean) = {
    flatMap(as)(e => if (f(e)) List(e) else Nil)
  }

  def `zip+`(one: List[Int], other: List[Int]): List[Int] = {
    @tailrec
    def i(oi: List[Int], otheri: List[Int], res: List[Int]): List[Int] = {
      oi match {
        case Nil =>
          res
        case Cons(h, t) =>
          otheri match {
            case Nil =>
              res
            case Cons(oh, ot) =>
              val nr = Cons(h + oh, res)
              i(t, ot, nr)
          }
      }
    }
    E3_12.reverse(i(one, other, Nil))
  }

  def zipWith[A, B](one: List[A], other: List[B]): List[(A, B)] = {
    @tailrec
    def i(oi: List[A], otheri: List[B], res: List[(A, B)]): List[(A, B)] = {
      oi match {
        case Nil =>
          res
        case Cons(h, t) =>
          otheri match {
            case Nil =>
              res
            case Cons(oh, ot) =>
              val nr = Cons((h, oh), res)
              i(t, ot, nr)
          }
      }
    }
    E3_12.reverse(i(one, other, Nil))
  }
}

object E {
  import scala.collection.immutable.{List => iL}
  def hasSubsequence[A](sup: iL[A], sub: iL[A]): Boolean = {
    sub.forall(sup.contains)
  }
}

object Check {
  def main(args: Array[String]): Unit = {
    assert(4 == E3_11.length(List(1, 2, 3, 4)))

    assert(List(2, 4, 6) == E3.`zip+`(List(1, 2, 3), List(1, 2, 3, 4)))
  }
}