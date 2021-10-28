package chapter2

import chapter2.Exercise23.curry

object Exercise24 {

  def uncurry[A,B,C](f: A => B => C): (A,B) => C =
    (a: A, b: B) => f(a)(b)

  def main(args: Array[String]): Unit = {
    val f1 = (a: Int, b: Int) => a+b*1.0
    val curry_out = curry[Int, Int, Double](f1);

    val uncurred = uncurry(curry_out)
    println(uncurred(10,2))
  }
}
