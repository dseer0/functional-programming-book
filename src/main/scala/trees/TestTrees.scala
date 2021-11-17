package trees

object TestTrees {

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
    def size[A](tree: Tree[A]): Int = {
      def go(tree: Tree[A]): Int = {
        tree match{
          case Leaf(v) => 1
          case Branch(x,xs) => go(x) + go(xs)
        }
      }
      go(tree)
    }
  }

  def main(args: Array[String]): Unit = {
    val tree: Tree[Int] = Branch(Branch(Leaf(3), Leaf(4)), Branch(Leaf(5), Leaf(6)))
    println(Tree.size(tree))

  }
}
