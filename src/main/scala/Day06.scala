import scala.io.Source

object Day06 extends App {

  val input = Source.fromInputStream(getClass.getResourceAsStream("/Day06.input")).getLines().toList

  val lights = new Array[Boolean](1000 * 1000)

  def index(x: Int, y: Int) = x + y * 1000

  type LightOperation = Boolean => Boolean

  def lightOn: LightOperation = b => true

  def lightOff: LightOperation = b => false

  def lightToggle: LightOperation = b => !b

  def brightnessAdjust(delta: Int)(value: Int) = Math.max(0, value + delta)

  val commandRegex = """(turn on|toggle|turn off) (\d{1,3}),(\d{1,3}) through (\d{1,3}),(\d{1,3})""".r

  object Command {
    def unapply(line: String) = {
      val commandRegex(command, x1, y1, x2, y2) = line
      Some(command, x1.toInt, y1.toInt, x2.toInt, y2.toInt)
    }
  }

  case class Command(command: String, x1: Int, y1: Int, x2: Int, y2: Int)

  val brightness = new Array[Int](1000 * 1000)

  input.foreach {
    case Command(command, x1, y1, x2, y2) =>

      val (lightOperation, brightnessOperation) = command match {
        case "turn on" => (lightOn, brightnessAdjust(1) _)
        case "turn off" => (lightOff, brightnessAdjust(-1) _)
        case "toggle" => (lightToggle, brightnessAdjust(2) _)
      }

      for (x <- x1 to x2) {
        for (y <- y1 to y2) {
          val idx = index(x, y)
          lights(idx) = lightOperation(lights(idx))
          brightness(idx) = brightnessOperation(brightness(idx))
        }
      }
  }

  println(lights.count(p => p))
  println(brightness.sum)
}
