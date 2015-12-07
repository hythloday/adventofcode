package advent


/**
  * Created by james on 03/12/2015.
  */
object Day1 extends Advent {
  def isUp(c: Char) = c == '('

  def part1: Int = {
    val (up, down) = input.mkString.partition(isUp)
    up.length - down.length
  }

  def part2: Int = {
    val runningFloor = input.mkString.scanLeft(0){ case (floor, op) =>
      if (isUp(op)) floor + 1
      else floor - 1
    }
    runningFloor.takeWhile(_ != -1).size
  }
}
