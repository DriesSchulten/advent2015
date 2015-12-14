import scala.annotation.tailrec
import scala.io.Source

object Day03 extends App {

  val input = Source.fromInputStream(getClass.getResourceAsStream("Day03.input")).mkString.toList

  case class Position(x: Int, y: Int)

  def nextPosition(d: Char, p: Position) = d match {
    case '^' => Position(p.x, p.y + 1)
    case '>' => Position(p.x + 1, p.y)
    case 'v' => Position(p.x, p.y - 1)
    case '<' => Position(p.x - 1, p.y)
  }

  @tailrec def followDirections(directions: List[Char], current: Position, positions: Set[Position]): Set[Position] = directions match {
    case c :: remaining =>
      val newCurrent = nextPosition(c, current)
      followDirections(remaining, newCurrent, positions + newCurrent)
    case Nil => positions
  }

  val start = Position(0, 0)
  val positions = followDirections(input, start, Set(start))
  println(positions.size)

  val directionsSplit = input.zipWithIndex.groupBy { case (c, i) => i % 2 == 0 } map { case (_, list) => list.map { case (c, _) => c } }
  val combinedPositions = directionsSplit.map(list => followDirections(list, start, Set(start))).foldLeft(Set[Position]()) { (s, positions) => s ++ positions }
  println(combinedPositions.size)
}
