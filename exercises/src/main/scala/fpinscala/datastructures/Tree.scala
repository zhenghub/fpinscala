package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = {
    t match {
      case l: Leaf[A] =>
        1
      case Branch(l, r) =>
        1 + size(l) + size(r)
    }
  }

  def maximum(t: Tree[Int]): Int = {
    t match {
      case Leaf(v) =>
        v
      case Branch(l, r) =>
        maximum(l) max maximum(r)
    }
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(lfv) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }


  def map[A, R](t: Tree[A], f: A => R): Tree[R] =
    t match {
      case Leaf(v) =>
        Leaf(f(v))
      case Branch(l, r) =>
        Branch(map(l, f), map(r, f))
    }

  def fold[A, B, C](t: Tree[A])(leaf: A => B)(m: (B, B) => B): B = {
    t match {
      case Leaf(v) =>
        leaf(v)
      case Branch(l, r) =>
        m(fold(l)(leaf)(m), fold(r)(leaf)(m))
    }
  }

  def size2[A](t: Tree[A]): Int = {
    fold(t)(_ => 1)(1 + _ + _)
  }

  def maximum2(t: Tree[Int]) = fold(t)(v => v)(_ max _)
  def depth2[A](t: Tree[A]) = fold(t)(_ => 0)((k, v) => 1 + (k max v))
  def map2[A, R](t: Tree[A], f: A => R) = fold(t)(v => Leaf(f(v)): Tree[R])(Branch(_, _))


}