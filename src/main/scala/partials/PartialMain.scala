package partials


object PartialMain {
  val sqrt: PartialFunction[Int, Double] = {
    case x if x >= 0 => Math.sqrt(x)
  }

  val toPrettyStr: PartialFunction[Any, String] = {
    case x: Int if x > 0 => s"positive: $x"
    case s: String => s
  }

  def main(args: Array[String]): Unit = {
    println("staart")
    //println(sqrt.orElse())
  }
}
