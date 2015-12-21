import scala.io.Source

object Day16 extends App {
  val input = Source.fromInputStream(getClass.getResourceAsStream("/Day16.input")).getLines().toList

  val regex = """Sue (\d+): (.*)""".r
  val data = input.map {
    case regex(num, properties) =>
      val props = properties.split(Array(',', ':')).map(_.trim).sliding(2, 2).map { case Array(key, value) => key -> value.toInt }.toMap
      num.toInt -> props
  }.toMap

  val desired = Map(
    "children" -> 3, "cats" -> 7, "samoyeds" -> 2, "pomeranians" -> 3, "akitas" -> 0,
    "vizslas" -> 0, "goldfish" -> 5, "trees" -> 3, "cars" -> 2, "perfumes" -> 1
  )

  val lt = (left: Int, right: Int) => left < right
  val gt = (left: Int, right: Int) => left > right
  val eq = (left: Int, right: Int) => left == right

  def filterData(comparators: Map[String, (Int, Int) => Boolean]) = {
    data.filter {
      case (id, properties) =>
        desired.map {
          case (property, value) => !properties.contains(property) || comparators.getOrElse(property, eq)(properties(property), value)
        }.forall(_ == true)
    }.keys
  }

  println(filterData(Map()).foreach(println(_)))

  val propertyComparators = Map("pomeranians" -> lt, "goldfish" -> lt, "cats" -> gt, "trees" -> gt)
  println(filterData(propertyComparators).foreach(println(_)))
}
