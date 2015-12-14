import scala.io.Source

object Day01 extends App {

  val input = Source.fromInputStream(getClass.getResourceAsStream("Day01.input")).mkString

  val floor = input.map {
    case '(' => 1
    case ')' => -1
  }

  val total = floor.sum
  println(total)

  var basementIndex: Option[Int] = None
  (floor.view.zipWithIndex foldLeft 0) { case (acc, (value, index)) =>
    val sum = acc + value
    if (sum < 0 && basementIndex.isEmpty) basementIndex = Some(index + 1)
    sum
  }

  basementIndex match {
    case Some(index) => println(index)
    case _ => println("No visit to basement found!")
  }
}