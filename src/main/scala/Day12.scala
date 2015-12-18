import org.json4s._
import org.json4s.native.JsonMethods._

object Day12 extends App {
  val input = parse(getClass.getResourceAsStream("Day12.input"))

  def sumNumbers(input: JValue): Int = input match {
    case JInt(x) => x.toInt
    case JArray(arr) => arr.map(sumNumbers).sum
    case JObject(fields) => fields.map(field => sumNumbers(field._2)).sum
    case _ => 0
  }

  println(sumNumbers(input))
}
