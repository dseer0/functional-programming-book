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

    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
      as match {
        case Nil => z
        case Cons(x, xs) => f(foldLeft(xs, z)(f), x)
      }
    }

    //    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    //      @annotation.tailrec
    //      def go[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    //        as match {
    //          case Nil => z
    //          case Cons(x, xs) => {
    //            go(xs, f(z, x))(f)
    //          }
    //        }
    //      }
    //
    //      go(as, z)(f)
    //    }

    def reverse[A](as: List[A]): List[A] = {

      def getLast(as: List[A]): A = {
        as match {
          case Cons(x, Nil) => x
          case Cons(x, xs) => getLast(xs)
        }
      }

      as match {
        case Nil => Nil
        case Cons(x, Nil) => Cons(x, Nil)
        case Cons(x, xs) => Cons(getLast(xs), reverse(List.init(as)))
      }

    }

    def length[A](as: List[A]): Int = {
      foldRight(as, 0)((a, b) => 1 + b)
    }

    def tail[A](list: List[A]): List[A] = {
      list match {
        case Nil => Nil
        case Cons(x, xs) => xs
      }
    }

    def flatten[A](list: List[List[A]]): List[A] = {
      foldRight(list, List[A]())((a, b) => List.appendList(a, b))
    }

    def drop[A](l: List[A], n: Int): List[A] = {
      @annotation.tailrec
      def dropinside(l: List[A], n: Int): List[A] = {
        if (n > 0) dropinside(List.tail(l), n - 1)
        else l
      }

      dropinside(l, n)
    }

    def append[A](list: List[A], a: A): List[A] = {
      list match {
        case Nil => Cons(a, Nil)
        case Cons(x, xs) => Cons(x, append(xs, a))
      }
    }

    def appendList[A](list: List[A], list2: List[A]): List[A] = {
      list match {
        case Nil => list2
        case Cons(x, xs) => Cons(x, appendList(xs, list2))
      }
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

    def transform[A](list: List[A])(f: A => A): List[A] = {
      list match {
        case Nil => Nil
        case Cons(x, xs) => Cons(f(x), transform(xs)(f))
      }
    }

    def doubleToString(list: List[Double]): List[String] = {
      list match {
        case Nil => Nil
        case Cons(x, xs) => Cons(x.toString, doubleToString(xs))
      }
    }

    def map[A, B](list: List[A])(f: A => B): List[B] = {
      list match {
        case Nil => Nil
        case Cons(x, xs) => Cons(f(x), map(xs)(f))
      }
    }

    def filter[A](as: List[A])(f: A => Boolean): List[A] = {
      as match {
        case Nil => Nil
        case Cons(x, xs) => if(f(x)) Cons(x,filter(xs)(f)) else filter(xs)(f)
      }
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }


  def main(args: Array[String]): Unit = {
    val z: List[Int] = List(1, 5, 3,4,3,5,32,5,245,5)
    val ld: List[Double] = List(1.3, 2.41321, 3.88)
    println(List.filter(z)(a => a != 5))

    //    println(List(1, 2, 3))
    //    println(List(3, 2, 1))
    println("-------------------")
    //println(List.foldRight(z, 0)((a, b) => a + b))
    //println(List.length(z))
    //println(List.length(List()))
    //    println(List.foldLeft(z, 0)((b, a) => b + a))
    //println(List.foldLeft(z, "a")((a, b) => a * b))

    //    val listOfLists = List(List("a", "b", "c"), List("d", "e", "f"), List("g", "h"))
    //    println(List.flatten(listOfLists))
    println(List.doubleToString(ld))
  }

  //append
  //reverse
  //list[list] -> list
}
