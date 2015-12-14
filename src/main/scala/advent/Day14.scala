package advent

import Math._

/**
  * Created by james on 14/12/2015.
  */
object Day14 extends Advent {

  case class Reindeer(name: String, speed: Int, exercise: Int, recuperate: Int) {
    def period = exercise + recuperate
  }

  def aReindeer = (ident <~ "can fly") ~ (wholeNumber <~ "km/s for") ~ (wholeNumber <~ "seconds, but then must rest for") ~ (wholeNumber <~ "seconds.") ^^ {
    case n~s~f~r => Reindeer(n, s.toInt, f.toInt, r.toInt)
  }

  lazy val reindeer = input.map(parse(aReindeer, _).get)

  def distance(seconds: Int)(r: Reindeer) = {
    val whole = r.speed * (seconds / r.period) * r.exercise
    val partial = r.speed * Math.min(seconds % r.period, r.exercise)
    whole + partial
  }

  def part1 = reindeer.map(distance(2503)).max
  def part2 = ???
}
