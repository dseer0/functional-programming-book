package immujson
import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

object IJt1 {

  sealed trait Payment

  case class CardPayment(id: Int, amount: BigDecimal, cardNumber: String) extends Payment
  case class CashPayment(id: Int, amount: BigDecimal) extends Payment
  case class AdditionToPayment(id: Int, payment: Payment) extends Payment


  def main(args: Array[String]): Unit = {

    val p1 = CardPayment(1, 3.14, "1111")
    val p2 = CashPayment(2, 41241)
    val p3 = AdditionToPayment(3, p2)
    val p4 = CashPayment(4, 41312)

    val listOfPayments: List[Payment] = List(p1,p2,p3,p4)

    println(listOfPayments.asJson)

  }

}
