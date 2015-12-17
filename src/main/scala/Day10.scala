import scala.annotation.tailrec
import scala.io.Source

object Day10 extends App {
  val input = Source.fromInputStream(getClass.getResourceAsStream("Day10.input")).mkString

  def run(line: String): String = {
    @tailrec def runAcc(line: List[Char], acc: StringBuilder): String = line match {
      case c :: Nil => acc.append(s"1$c").toString
      case c :: rest =>
        val length = rest.takeWhile(_ == c).size
        runAcc(rest.drop(length), acc.append(s"${length + 1}$c"))
      case Nil => acc.toString
    }

    runAcc(line.toList, new StringBuilder)
  }

  val res = (0 until 40).foldLeft(input) { case (acc, _) => run(acc) }
  println(res.length)
  val res2 = (0 until 10).foldLeft(res) { case (acc, _) => run(acc) }
  println(res2.length)
}
