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

  def step(molecule: String): String = {
    for {
      (lhs, rhs) <- transitions
      out <- transition(rhs, lhs, molecule)
    } yield out
  }.sortBy(_.length).head

  def analyse = {
    lazy val stream: Stream[String] = Stream.cons(molecule, stream.map(step))
    stream
  }

  def part1 = transitions.flatMap{ case (l, r) => transition(l, r, molecule) }.distinct.size
  def part2 = analyse.takeWhile(s => !(s contains "e")).size
}
