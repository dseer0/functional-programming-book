package implicits

case class Person(name: String, age: Int)

object SortTest {



  def main(args: Array[String]): Unit = {

    implicit val bb = new A()

    val list = List(
      Person("Alan", 15),
      Person("AAA", 347),
      Person("AAA", 12),
      Person("ABA", 23),
      Person("ZZZZ", 14),
    )

    val listSorted = list.sorted
    println(listSorted)

  }


}

class A extends Ordering[Person] {
  override def compare(x: Person, y: Person): Int = {
    if (x.age % 2 == 0)  {
      if(x.age > y.age) return 1
      -1
    }
    else 1
  }
}
