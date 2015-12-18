import org.json4s._
import org.json4s.native.JsonMethods._

object Day12 extends App {
  val input = parse(getClass.getResourceAsStream("Day12.input"))

  def sumNumbers(json: JValue, fieldFilter: (JObject => Boolean)): Int = json match {
    case JInt(x) => x.toInt
    case JArray(arr) => arr.map(sumNumbers(_, fieldFilter)).sum
    case obj @ JObject(fields) if fieldFilter(obj) => fields.map(field => sumNumbers(field._2, fieldFilter)).sum
    case _ => 0
  }

  println(sumNumbers(input, _ => true))
  println(sumNumbers(input, obj => !obj.values.exists {
    case (_, value) => value == "red"
  }))
}
