package chapter2

object Exercise23 {

  def curry[A, B, C](f: (A,B) => C): A => (B => C) =
    (a: A) =>  (b: B) => f(a,b)

  def main(args: Array[String]): Unit = {
    val f1 = (a: Int, b: Int) => a+b*1.0
    val curry_out = curry[Int, Int, Double](f1);


    val out = curry_out.apply(5)
    val outofOut = out.apply(3)
    println(outofOut)

    println(curry_out(10)(2))
  }
}
