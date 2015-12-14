import scala.collection.mutable
import scala.io.Source

object Day07 extends App {
  val input = Source.fromInputStream(getClass.getResourceAsStream("/Day07.input")).getLines().toList

  val regex = """([\d]+|[a-z]+)? ?([A-Z]+)? ?([\d]+|[a-z]+)? -> ([a-z]+)""".r

  case class Gate(gate: Option[String], inputA: Wire, inputB: Option[Wire])

  case class Wire(var value: Option[Int], var gate: Option[Gate]) {
    lazy val calcValue: Int = value match {
      case Some(x) => x
      case _ => gate match {
        case Some(g) => processGate(g)
        case _ => throw new IllegalStateException("Unsolvable")
      }
    }
  }

  var wires = mutable.Map[String, Wire]()

  def createWire(input: String): Wire = input match {
    case numeric if numeric.forall(_.isDigit) => Wire(Some(numeric.toInt), None)
    case _ => wires.getOrElseUpdate(input, Wire(None, None))
  }

  def createModel() = {
    wires.clear()
    input foreach {
      case regex(inputA, null, null, wire) => createWire(wire).gate = Some(Gate(None, createWire(inputA), None))
      case regex(null, gate, inputB, wire) => createWire(wire).gate = Some(Gate(Some(gate), createWire(inputB), None))
      case regex(inputA, gate, inputB, wire) => createWire(wire).gate = Some(Gate(Some(gate), createWire(inputA), Some(createWire(inputB))))
    }
  }

  def processGate(gate: Gate): Int = {
    gate match {
      case Gate(None, left, None) => left.calcValue
      case Gate(Some("NOT"), value, None) => ~value.calcValue & 0xFFFF
      case Gate(Some("AND"), left, Some(right)) => left.calcValue & right.calcValue
      case Gate(Some("OR"), left, Some(right)) => left.calcValue | right.calcValue
      case Gate(Some("LSHIFT"), left, Some(right)) => left.calcValue << right.calcValue
      case Gate(Some("RSHIFT"), left, Some(right)) => left.calcValue >> right.calcValue
    }
  }

  createModel()
  val a = wires("a").calcValue
  println(a)

  createModel()
  wires("b").value = Some(a)
  println(wires("a").calcValue)
}
