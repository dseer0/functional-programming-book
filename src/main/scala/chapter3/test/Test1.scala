package chapter3.test

object Test1 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
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
        else  l
      }

      dropinside(l, n)
    }

    def setHead[A](list: List[A], value: A): List[A] = {
      list match {
        case Nil => Cons(value, Nil)
        case Cons(x, xs) => Cons(value, Cons(x, xs))
      }
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

  def main(args: Array[String]): Unit = {
    val z = List(1,2,3,4,5,6,7,8,9)

    println(List.drop(z, 4))

  }

}
