import scala.annotation.tailrec
import scala.io.Source

object Day10 extends App {
  val input = Source.fromInputStream(getClass.getResourceAsStream("Day10.input")).mkString

  def run(line: List[Char]): List[Char] = {
    @tailrec def runAcc(line: List[Char], acc: List[Char]): List[Char] = line match {
      case c :: rest =>
        val length = rest.takeWhile(_ == c).size + 1
        runAcc(rest.drop(length - 1), acc ++ length.toString.toList ++ List(c))
      case Nil => acc
    }

    runAcc(line, List())
  }

  var str = input.toList
  (0 to 40).foreach(_ => str = run(str))
  println(str.size)
}
