package advent

/**
  * Created by james on 16/12/2015.
  */
object Day16 extends Advent {

  type Sue = Map[String, Int]

  def thing = "children" | "cats" | "samoyeds" | "pomeranians" | "akitas" | "vizslas" | "goldfish" | "trees" | "cars" | "perfumes"
  def aSue = ("Sue" ~> wholeNumber <~ ":") ~
    (thing <~ ":") ~ (wholeNumber <~ ",") ~
    (thing <~ ":") ~ (wholeNumber <~ ",") ~
    (thing <~ ":") ~ wholeNumber ^^ { case n~t1~q1~t2~q2~t3~q3 => n -> Map(t1 -> q1.toInt, t2 -> q2.toInt, t3 -> q3.toInt) }

  lazy val sues = input.map(parse(aSue, _).get).toMap

  lazy val cardSue = Map("children" -> 3, "cats" -> 7, "samoyeds" -> 2, "pomeranians" -> 3,
    "akitas" -> 0, "vizslas" -> 0, "goldfish" -> 5, "trees" -> 3, "cars" -> 2, "perfumes" -> 1)

  def canBeSender(b: Sue) = cardSue.filter{ case (k, v) => b.keys.toSeq contains k } == b

  def part1 = sues.collect{ case (n, sue) if canBeSender(sue) => n }
  def part2 = ???
}
