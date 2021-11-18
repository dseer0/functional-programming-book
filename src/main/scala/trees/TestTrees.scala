package trees

object TestTrees {

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
    def size[A](tree: Tree[A]): Int = {
      def go(tree: Tree[A]): Int = {
        tree match {
          case Leaf(v) => 1
          case Branch(x, xs) => go(x) + go(xs)
        }
      }

      go(tree)
    }

    def maximum(tree: Tree[Int]): Int = {
      def go(tree: Tree[Int]): Int = {
        tree match {
          case Leaf(v) => v
          case Branch(x, xs) => go(x).max(go(xs))
        }
      }

      go(tree)
    }

    def depth[A](tree: Tree[A]): Int = {
      def go(tree: Tree[A], depth: Int): Int = {
        tree match {
          case Leaf(v) => depth
          case Branch(x, xs) => go(x, depth + 1).max(go(xs, depth + 1))
        }
      }
        go(tree, 0)
      }
    }

    def main(args: Array[String]): Unit = {
      val tree: Tree[Int] = Branch(
        Branch(Leaf(3), Leaf(4)),
        Branch(Leaf(5),
          Branch(Leaf(12), Leaf(44))
        )
      )

      val tree2: Tree[Int] = Branch(Branch(Leaf(4), Leaf(5)), Leaf(5))


      println(Tree.size(tree))
      println(Tree.maximum(tree))
      println(Tree.depth(tree2))

    }
  }
