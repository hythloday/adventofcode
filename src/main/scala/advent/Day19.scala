package advent

/**
  * Created by james on 19/12/2015.
  */
object Day19 extends Advent {

  def aTransition = (ident <~ "=>") ~ ident ^^ { case l~r => l -> r }
  def puzzle = (rep(aTransition) ~ ident) ^^ { case ts~m => (ts, m) }

  lazy val (transitions, molecule) = parse(puzzle, input.mkString("\n")).get

  def transition(from: String, to: String, s: String) = s.sliding(from.length).zipWithIndex.collect {
    case (`from`, x) => s.take(x) + to + s.drop(x + from.length)
  }

  def part1 = transitions.flatMap{ case (l, r) => transition(l, r, molecule) }.distinct.size
  def part2 = ???
}
