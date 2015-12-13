package advent

/**
  * Created by james on 13/12/2015.
  */
object Day13 extends Advent {

  def happiness = (ident <~ "would") ~ ("lose" | "gain") ~ wholeNumber ~ ("happiness units by sitting next to" ~> ident <~ ".") ^^ {
    case a~"lose"~n~b => (a, b) -> -n.toInt
    case a~"gain"~n~b => (a, b) -> n.toInt
  }

  lazy val moods = input.map(parse(happiness, _).get).toMap.withDefaultValue(0)
  lazy val people = moods.keys.flatMap{ case (a, b) => List(a, b) }.toList
  def happinessPair(ps: List[String]) = moods((ps.head, ps.last)) + moods((ps.last, ps.head))
  def hScore(people: List[String]) =  people.sliding(2).map(happinessPair).sum + happinessPair(List(people.head, people.last))

  def part1 = people.permutations.map(hScore).max
  def part2 = ("You" :: people).permutations.map(hScore)

}
