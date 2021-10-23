package chapter2

object Exercise21 {

  def fib(n: Int): Int = {
    def getNum(a: Int, b: Int, steps: Int): Int ={
      if(steps == 0) a
      else
        getNum(b, a+b, steps-1)

    }

    getNum(0, 1, n)

  }
  //0 1 1 2 3 5
  def main(args: Array[String]): Unit = {
    Range(0,10).foreach(i => println(i + "=>" + fib(i)))
  }
}
