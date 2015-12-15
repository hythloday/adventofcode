package advent

/**
  * Created by james on 15/12/2015.
  */
object Day15 extends Advent {

  case class Ingredient(name: String, qualities: List[Long], calories: Long)

  def anIngredient = ident ~ (": capacity" ~> wholeNumber) ~ (", durability" ~> wholeNumber) ~
    (", flavor" ~> wholeNumber) ~ (", texture" ~> wholeNumber) ~ (", calories" ~> wholeNumber) ^^
    { case n~c~d~f~t~cl => Ingredient(n, List(c.toInt, d.toInt, f.toInt, t.toInt), cl.toInt) }

  lazy val ingredients = input.map(parse(anIngredient, _).get)

  def bake(recipe: Map[Int, Ingredient]) = recipe.map { case (qty, ingredient) => ingredient.qualities.map(_*qty) }
    .transpose.map(_.sum).map(Math.max(0, _)).product

  def calories(recipe: Map[Int, Ingredient]) = recipe.map { case (qty, ingredient) => ingredient.calories*qty }.sum

  def split4(i: Int) = for {
    a <- 0 to i
    b <- 0 to i - a
    c <- 0 to i - (a + b)
    d = i - (a + b + c)
  } yield Seq(a, b, c, d)

  def part1 = split4(100).map(qtys => qtys.zip(ingredients).toMap).map(bake).max
  def part2 = split4(100).map(qtys => qtys.zip(ingredients).toMap).filter(r => calories(r) == 500).map(bake).max
}
