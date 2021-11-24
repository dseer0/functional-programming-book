package eithers

object EitherTest {

  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = {
      this match {
        case Left(l) => Left(l)
        case Right(r) => Right(f(r))
      }
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
      this match {
        case Left(l) => Left(l)
        case Right(r) => f(r)
      }
    }

    //TODO rightbiased?
    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
      this match {
        case Left(l) => b
        case Right(r) => Right(r)
      }
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
      this match {
        case Left(l) => Left(l)
        case Right(r) => Right(r).flatMap(a => b.map(b => f(a, b)))
      }
    }
  }

  case class Left[+E](value: E) extends Either[E, Nothing]

  case class Right[+A](value: A) extends Either[Nothing, A]

  object Either {
    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
      es match {
        case Nil => Right(Nil) //TODO
        case ::(head, next) => {
          head.flatMap(el => sequence(next).map(el :: _))
        }
      }
    }

    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
      Either.sequence(as.map(f(_)))//TODO could be better
    }
  }

}
