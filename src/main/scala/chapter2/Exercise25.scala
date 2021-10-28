package chapter2

import chapter2.Exercise23.curry

object Exercise25 {

  def compose[A,B,C](f: B=>C, g: A => B): A => C =
    (a: A) => f(g(a))

  def main(args: Array[String]): Unit = {
  }
}
