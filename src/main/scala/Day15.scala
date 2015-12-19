import scala.io.Source

object Day15 extends App {
  val input = Source.fromInputStream(getClass.getResourceAsStream("/Day15.input")).getLines().toList

  val regex = """(\w+): capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)""".r

  case class Ingredient(name: String, capacity: Int, durability: Int, flavor: Int, texture: Int, calories: Int) {
    def getParams = List(capacity, durability, flavor, texture)
  }

  val ingredients = input.map {
    case regex(name, capacity, durability, flavor, texture, calories) =>
      Ingredient(name, capacity.toInt, durability.toInt, flavor.toInt, texture.toInt, calories.toInt)
  }.toList

  def calculatePermutations(num: Int, max: Int): List[List[Int]] = {
    if (num <= 1) Nil
    else if (num == 2)
      for {
        x <- List.range(0, max + 1)
      } yield List(x, max - x)
    else
      for {
        x <- List.range(0, max + 1)
        sub <- calculatePermutations(num - 1, max - x)
      } yield x :: sub
  }

  val ingredientPermutations = ingredients.permutations.toList
  val amountPermutations = calculatePermutations(ingredients.length, 100)

  val scores = for {
    ingredientPermutation <- ingredientPermutations
    amountPermutation <- amountPermutations
  } yield {
    val combination = ingredientPermutation.zip(amountPermutation)
    val score = (0 until 4).map { i => combination.map { case (ing, am) => ing.getParams(i) * am }.sum.max(0) }.product
    val calories = combination.map { case (ing, am) => ing.calories * am }.sum.max(0)
    (score, calories)
  }

  println(scores.map { case (s, _) => s }.max)
  println(scores.filter { case (s, c) => c == 500 }.map { case (s, _) => s }.max)
}
