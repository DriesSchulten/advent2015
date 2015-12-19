import scala.io.Source

object Day08 extends App {
  val input = Source.fromInputStream(getClass.getResourceAsStream("/Day08.input")).getLines().toList

  def unescape(line: String): Int = {
    val iter = line.iterator
    var res = ""
    while (iter.hasNext) {
      iter.next() match {
        case '\\' =>
          iter.next() match {
            case c if c == '"' || c == '\\' => res += c
            case 'x' => res += Integer.parseInt(iter.next().toString + iter.next(), 16).toChar
          }
        case c => res += c
      }
    }
    res.length - 2
  }

  def escape(input: String): Int = {
    input.flatMap {
      case c if c == '"' || c == '\\' => "\\" + c
      case c => c.toString
    }.length + 2
  }

  println(input.map { l =>
    l.length - unescape(l)
  }.sum)

  println(input.map { l =>
    escape(l) - l.length
  }.sum)
}
