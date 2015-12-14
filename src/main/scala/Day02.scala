import scala.io.Source

object Day02 extends App {

  val input = Source.fromInputStream(getClass.getResourceAsStream("/Day02.input")).getLines().toList
  val presents = input.map(l => toPresent(l))

  def toPresent(line: String) = line.split("x").toList.map(s => s.toLong)

  val paperNeeded = presents.foldLeft(0L) {
    case (amount, p) =>
      val area = p match {
        case l :: w :: h :: Nil => List(l * w, w * h, h * l)
        case _ => List()
      }
      amount + area.map(x => x * 2).sum + area.min
  }

  println(paperNeeded)

  val ribbonNeeded = presents.foldLeft(0L) {
    case (amount, p) =>
      val bow = p.product
      val shortest = p.sorted.take(2).map(n => n + n).sum
      amount + bow + shortest
  }

  println(ribbonNeeded)
}
