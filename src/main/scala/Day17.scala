import scala.io.Source

object Day17 extends App {
  val input = Source.fromInputStream(getClass.getResourceAsStream("/Day17.input")).getLines().toList

  val containers = input.map(_.toInt)

  def calculateCombinations(capacity: Int, containers: List[Int]): List[List[Int]] = {
     for {
       n <- (1 to containers.size).toList
       combination <- containers.zipWithIndex.combinations(n) if combination.map(_._1).sum == capacity
     } yield combination.map(_._1)
  }

  val combinations = calculateCombinations(150, containers)
  println(combinations.size)

  val min = combinations.map(_.size).min
  println(combinations.count(_.size == min))
}
