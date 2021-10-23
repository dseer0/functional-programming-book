package chapter2

object Exercise22 {

  def isSorted[A](orderedList: Array[A], as: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def check(index: Int): Boolean = {
      if (index + 1 >= orderedList.length) return true
      if (!as(orderedList(index), orderedList(index + 1))) return false
      check(index + 1)
    }

    check(0)
  }

  def main(args: Array[String]): Unit = {
    val sortingFunction = (a: Int, b: Int) => a > b

    println(isSorted(Array(3,4,5), sortingFunction))
    println(isSorted(Array(5,4,3), sortingFunction))
    println(isSorted(Array(3,4,3), sortingFunction))

  }
}
