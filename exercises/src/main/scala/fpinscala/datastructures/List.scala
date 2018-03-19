package fpinscala.datastructures

import fpinscala.datastructures.List._

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }


  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  // 3.1 - Should return 3
    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }

  // 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("List is empty")
    case Cons(_, xs) => xs
  }

  // 3.3
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("List is empty")
    case Cons(_, xs) => Cons(h, xs)
  }

  // 3.4
  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if(n <= 0) l
    else {
      l match {
        case Nil => Nil
        case Cons(_, xs) => drop(xs, n - 1)
      }
    }
  }

  // 3.5
  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(x, xs) if(f(x)) => dropWhile(xs, f)
      case _ => l
    }
  }

  // 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("Can not call init on empty list")
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  // 3.7 No

  // 3.8 Same list

  // 3.9
  def length[A](l: List[A]): Int =
    foldRight(l, 0) ((_, acc) => acc + 1)

  // 3.10
  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  // 3.11
  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  // 3.12
  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, List[A]())((acc, a) => Cons(a, acc))
  }

  // TODO: 3.13 - Implement foldRight in terms of fold left

  // 3.14
  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((x, xs) => Cons(x, xs))

  // 3.15
  def concat[A](xs: List[List[A]]): List[A] = {
    foldRight(xs, List[A]())(append)
  }

  // 3.16
  def add1(l: List[Int]): List[Int] = {
    foldRight(l, List[Int]())((h, t) => Cons(h + 1, t))
  }

  // 3.17
  def doubleToString(l: List[Double]): List[String] = {
    foldRight(l, List[String]())((h, t) => Cons(h.toString, t))
  }

  // 3.18
  // Not stack safe
  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, List[B]())((h, t) => Cons(f(h), t))

}

object Test {
  def main(args: Array[String]): Unit = {
    println(s"${doubleToString(List(1, 2, 3))}")
  }
}

