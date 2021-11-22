package options

object OptionTest {

  trait Option[+A] {
    def map[B](f: A => B): Option[B] = {
      this match {
        case None => None
        case Some(a) => Some(f(a))
      }
    }

    def getOrElse[B >: A](default: => B): B = {
      this match {
        case None => default
        case Some(a) => a
      }
    }

    def flatMap[B](f: A => Option[B]): Option[B] = {
      map(f).getOrElse(None)
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = {
      map(a => Some(a)).getOrElse(ob)
    }

    def filter(f: A => Boolean): Option[A] = {
      flatMap(a => if (f(a)) Some(a) else None)
    }
  }

  case class Some[+A](get: A) extends Option[A]

  case object None extends Option[Nothing]

  object Option {
    def variance(xs: Seq[Double]): Option[Double] = {
      //m = mean
      //mean(math.pow(a-m,2)
      mean(xs)
        .flatMap(m => mean(xs.map(el => math.pow(el - m, 2))))
    }

    def mean(xs: Seq[Double]): Option[Double] = {
      if (xs.isEmpty) None
      else Some(xs.sum / xs.size)
    }
  }

  def main(args: Array[String]): Unit = {
    val op = Some(Some(Some(Some(3))))
    val op2 = op.flatMap(a => a).flatMap(a => a).flatMap(a => a)
    println(op2)
  }

}
