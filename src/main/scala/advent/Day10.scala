package advent

/**
* Created by james on 10/12/2015.
*/
object Day10 extends Advent {

  lazy val seed = "1321131112"

  def look(s: String) = s.foldLeft(List.empty[(Char, Int)]) {
    case ((chr, count) :: tail, c) if chr == c => (chr, count +1) :: tail
    case (xs, c) => (c, 1) :: xs
  }
  def say(c: Char, i: Int) = s"$i$c"

  def lookAndSay(s: String) = look(s).reverse.map((say _).tupled).mkString

  def part1 = Function.chain(List.fill(40)(lookAndSay _))(seed).length
  def part2 = Function.chain(List.fill(50)(lookAndSay _))(seed).length
}
