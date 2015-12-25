package advent

import scala.annotation.tailrec

/**
  * Created by james on 25/12/2015.
  */
object Day25 extends Advent {

  def valueOf(row: Int, col: Int) = {
    val seed = 20151125L
    val cell = 1 -> 1
    @tailrec
    def calculate(address: (Int, Int), value: Long): Long = address match {
      case (`row`, `col`) => value
      case _ => calculate(nextCell(address), nextValue(value))
    }
    calculate(cell, seed)
  }

  def nextCell(address: (Int, Int)) = address match {
    case (1, col) => (col+1) -> 1
    case (row, col) => (row-1) -> (col+1)
  }
  def nextValue(previous: Long) = (previous * 252533L) % 33554393L

  def part1 = valueOf(2947, 3029)
  def part2 = ???
}
