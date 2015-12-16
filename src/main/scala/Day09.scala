import scala.io.Source

object Day09 extends App {
  val input = Source.fromInputStream(getClass.getResourceAsStream("/Day09.input")).getLines().toList

  val regex = """(\w+) to (\w+) = (\d+)""".r

  val distances = input.map {
    case regex(start, end, distance) => (start :: end :: Nil) -> distance.toInt
  }.toMap

  val costs = distances.keySet.flatten.toList.permutations.map { p =>
    p.sliding(2).map(list => distances.getOrElse(list, distances(list.reverse))).sum
  }.toList

  println(costs.min)
  println(costs.max)
}
