import scala.io.Source

object Day05 extends App {

  val input = Source.fromInputStream(getClass.getResourceAsStream("Day05.input")).getLines().toList

  val vowels = "aeiou".toList

  def vowelCount(str: String): Boolean = str.count(c => vowels.contains(c)) >= 3

  def letterDouble(str: String, length: Int): Boolean = str.sliding(1 + length).count(s => s(0) == s(length)) > 0

  val disallowed = "ab" :: "cd" :: "pq" :: "xy" :: Nil

  def noIllegal(str: String): Boolean = !disallowed.exists(pattern => str.contains(pattern))

  def niceStrings = input.count(str => vowelCount(str) && letterDouble(str, 1) && noIllegal(str))

  println(niceStrings)

  def hasPair(str: String): Boolean = str.sliding(2).exists(s => str.lastIndexOf(s) > str.indexOf(s) + 1)

  def nicerStrings = input.count(str => hasPair(str) && letterDouble(str, 2))

  println(nicerStrings)
}
