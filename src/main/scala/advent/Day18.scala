package advent

/**
  * Created by james on 18/12/2015.
  */
object Day18 extends Advent {

  type Grid = Map[(Int, Int), Boolean]
  def state = rep("#" ^^^ { true } | "." ^^^ { false })

  lazy val state0 = {
    for {
      (line, lineNo) <- input.map(parse(state, _).get).zipWithIndex
      (char, charNo) <- line.zipWithIndex
    } yield (lineNo, charNo) -> char
  }.toMap.withDefaultValue(false)

  def neighbours(x: Int, y: Int) = for {
    offX <- -1 to 1
    offY <- -1 to 1
    if offX != 0 || offY != 0
  } yield (x + offX) -> (y + offY)

  def next: (Boolean, Int) => Boolean = {
    case (true, n) if n == 2 || n == 3 => true
    case (true, _) => false
    case (false, 3) => true
    case (false, _) => false
  }

  def simulate(g: Grid): Grid = {
    for {
      x <- 0 until 100
      y <- 0 until 100
    } yield x -> y -> next(g(x -> y), neighbours(x, y).count(g))
  }.toMap.withDefaultValue(false)

  lazy val simulate100 = Function.chain((1 to 100).map(i => simulate _))
  def part1 = simulate100(state0).values.count(identity)

  def part2 = ???
}
