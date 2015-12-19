import scala.io.Source

object Day13 extends App {
  val input = Source.fromInputStream(getClass.getResourceAsStream("/Day13.input")).getLines().toList

  val seating = """(\w+) would (gain|lose) (\d+) happiness units by sitting next to (\w+).""".r
  var people = input.map({
    case seating(p1, gainOrLose, amount, p2) => (p1, p2) -> amount.toInt * (if (gainOrLose == "gain") 1 else -1)
  }).toMap

  def calculateHapiness(configuration: List[String]): Int = {
    configuration.sliding(2).map { persons =>
      val tuple = persons match {
        case List(a, b) => (a, b)
      }
      people(tuple) + people(tuple.swap)
    }.sum
  }

  val participants = people.flatMap { case ((a, b), _) => List(a, b) }.toList.distinct
  println(participants.permutations.map(x => x.last :: x).map(calculateHapiness).max)

  people = people ++ participants.flatMap(p => Map(("me", p) -> 0, (p, "me") -> 0))
  val participants2 = people.flatMap { case ((a, b), _) => List(a, b) }.toList.distinct
  println(participants2.permutations.map(x => x.last :: x).map(calculateHapiness).max)
}