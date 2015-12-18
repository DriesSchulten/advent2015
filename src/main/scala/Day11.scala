import scala.io.Source

object Day11 extends App {
  val input = Source.fromInputStream(getClass.getResourceAsStream("Day11.input")).mkString

  def increase(password: String): String = {
    val min = 'a'.toInt
    val max = 'z'.toInt

    def run(elem: List[Int]): List[Int] = elem match {
      case Nil => Nil
      case c :: Nil if c < max => List(c + 1)
      case c :: Nil => List(min, min)
      case c :: t if c < max => c + 1 :: t
      case h :: t => min :: run(t)
    }

    run(password.reverse.map(_.toInt).toList).reverse.map(_.toChar).mkString
  }

  def hasIncreasing(password: String): Boolean = {
    password.sliding(3).exists(str => str.map(_.toInt).toList.sliding(2).map {
      case List(a, b) => b - a
    }.toList.forall(_ == 1))
  }

  val notAllowed = Set('i', 'o', 'l')
  def containsOnlyAllowed(password: String): Boolean = !notAllowed.exists(c => password.contains(c))

  def containsPairs(password: String): Boolean = {
    val pairs = password.sliding(2).toSeq
    pairs.lastIndexWhere(l => l(0) == l(1)) - pairs.indexWhere(l => l(0) == l(1)) > 1
  }

  def genNewPassword(password: String) = {
    Stream.iterate(password)(increase).dropWhile(password => !(hasIncreasing(password) &&
      containsOnlyAllowed(password) && containsPairs(password))).head
  }

  val newPassword = genNewPassword(input)
  println(newPassword)
  println(genNewPassword(increase(newPassword)))
}
