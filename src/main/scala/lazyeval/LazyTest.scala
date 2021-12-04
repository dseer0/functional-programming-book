package lazyeval

import scala.annotation.tailrec
import scala.util.{Random, Try}


object LazyTest {
  def roll(): Int = {
    val n = Random.nextInt(6) + 1
    println(s"Rolled $n ")
    if (n < 5) throw new IllegalStateException("fail, rolled " + n)
    else n
  }

  @tailrec
  def retry[T](n: Int, operation: =>  T): Try[T] = {
    val result = Try(operation)
    if (result.isFailure && n > 0) retry(n - 1, operation)
    else result
  }


  def main(args: Array[String]): Unit = {
    retry[Int](10, roll())

    Try(5)
    Try(() => 3 + 6)
  }
}
