import scala.io.Source

object Day13 extends App {
  val input = Source.fromInputStream(getClass.getResourceAsStream("/Day13.input")).getLines().toList

  val seating = """(\w+) would (gain|lose) (\d+) happiness units by sitting next to (\w+).""".r
  val people = input.map({
      case seating(p1, gainOrLose, amount, p2) => (p1, p2) -> amount.toInt * (if (gainOrLose == "gain") 1 else -1)
  }).toMap

  val participants = people.flatMap { case ((a, b), _) => List(a, b) }.toList.distinct
}