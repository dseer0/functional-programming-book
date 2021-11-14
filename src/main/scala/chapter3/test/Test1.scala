package chapter3.test

import scala.runtime.Nothing$

object Test1 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    //    def sum(ints: List[Int]): Int = ints match {
    //      case Nil => 0
    //      case Cons(x, xs) => x + sum(xs)
    //    }
    //
    //    def product(ds: List[Double]): Double = ds match {
    //      case Nil => 1.0
    //      case Cons(0.0, _) => 0.0
    //      case Cons(x, xs) => x * product(xs)
    //    }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }
    }

    def length[A](as: List[A]): Int = {
      foldRight(as, 0)((a, b) => b + 1)
    }

    def tail[A](list: List[A]): List[A] = {
      list match {
        case Nil => Nil
        case Cons(x, xs) => xs
      }
    }

    def drop[A](l: List[A], n: Int): List[A] = {
      @annotation.tailrec
      def dropinside(l: List[A], n: Int): List[A] = {
        if (n > 0) dropinside(List.tail(l), n - 1)
        else l
      }

      dropinside(l, n)
    }

    def setHead[A](list: List[A], value: A): List[A] = {
      list match {
        case Nil => Cons(value, Nil)
        case Cons(x, xs) => Cons(value, Cons(x, xs))
      }
    }

    def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
      @annotation.tailrec
      def go(l: List[A], f: A => Boolean): List[A] = {
        l match {
          case Nil => l
          case Cons(x, xs) => if (f(x)) go(List.tail(l), f) else l
        }
      }

      go(l, f)
    }

    def init[A](l: List[A]): List[A] = {
      l match {
        case Nil => Nil
        case Cons(x, Nil) => Nil
        case Cons(x, xs) => Cons(x, init(xs))
      }
    }

    def add[A](l: List[A], a: A): List[A] = {
      l match {
        case Nil => Cons(a, Nil)
        case Cons(x, xs) => Cons(x, add(xs, a))
      }
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

  def main(args: Array[String]): Unit = {
    val z: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9)

    println(List.foldRight(z, 0)((a, b) => a + b))
    println(List.length(z))
    println(List.length(List()))

  }

}
