package advent

import scala.util.parsing.combinator.JavaTokenParsers

/**
  * Created by james on 09/12/2015.
  */
object Day9 extends Advent with JavaTokenParsers {

  def line = (ident <~ "to") ~ ident ~ ("=" ~> wholeNumber) ^^ { case a~b~c => Set(a,b) -> c.toInt }

  lazy val routes = input.map(parse(line, _).get).toMap
  lazy val cities = routes.keys.flatten
  lazy val paths = cities.toList.permutations.toList
  def length(path: List[String]) = path.sliding(2).map { case pair => routes(pair.toSet) }.sum

  def part1 = paths.map(length).min
  def part2 = paths.map(length).max
}
