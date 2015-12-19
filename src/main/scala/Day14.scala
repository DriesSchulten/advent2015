import scala.io.Source
import scala.collection.mutable

object Day14 extends App {
  val input = Source.fromInputStream(getClass.getResourceAsStream("/Day14.input")).getLines().toList
  val secondsToRun = 2503

  val regex = """(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.""".r

  case class Reindeer(name: String, speed: Int, active: Int, rest: Int)

  val reindeers = input.map {
    case regex(name, speed, active, rest) => Reindeer(name, speed.toInt, active.toInt, rest.toInt)
  }

  def calculatePosition(reindeer: Reindeer, secondsLeft: Int): Int = {
    if (secondsLeft > 0) reindeer.speed * secondsLeft.min(reindeer.active) +
      calculatePosition(reindeer, secondsLeft - (reindeer.active + reindeer.rest))
    else 0
  }

  var points = mutable.Map(reindeers.map(r => r -> 0).toMap.toSeq: _*)
  def calculatePoints() = {
    def pointsAfterSeconds(secondsElapsed: Int): Unit = {
      if (secondsElapsed < secondsToRun) {
        val positions = reindeers.map(r => r -> calculatePosition(r, secondsElapsed)).toMap
        val max = positions.values.max
        positions.filter { case (r, pos) => pos == max }.foreach { case (r, _) => points(r) += 1 }
        pointsAfterSeconds(secondsElapsed + 1)
      }
    }
    pointsAfterSeconds(1)
  }

  println(reindeers.map(r => calculatePosition(r, secondsToRun)).max)
  calculatePoints()
  println(points.values.max)
}
