package advent

/**
  * Created by james on 03/12/2015.
  */
object Day3 extends Advent {

  case class Coord(x: Int, y: Int) {
    def move(c: Char) = c match {
      case '^' => copy(y = y + 1)
      case 'v' => copy(y = y - 1)
      case '>' => copy(x = x + 1)
      case '<' => copy(x = x - 1)
    }
  }

  def houses(moves: List[Char]) = moves.scanLeft(Coord(0, 0)) { case (last, dir) =>
    last.move(dir)
  }

  def part1 = {
    houses(input.mkString.toList).distinct.size
  }

  def part2 = {
    val (santa, robo) = input.mkString.toList.grouped(2).map{ case List(a, b) => (a, b) }.toList.unzip
    (houses(santa) ::: houses(robo)).distinct.size
  }
}
