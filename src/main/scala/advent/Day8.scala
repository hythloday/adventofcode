package advent

import scala.util.parsing.combinator.JavaTokenParsers

/**
  * Created by james on 07/12/2015.
  */
object Day8 extends Advent with JavaTokenParsers {

  def bs = "\\"
  def quot = "\""
  def escBs = bs ~ bs
  def escQuot = bs ~ quot
  def escChr = bs ~ "x[0-9a-f][0-9a-f]".r
  def oneChr = "[a-z]".r
  def char = escBs | escQuot | escChr | oneChr
  def escapedLine = {
    quot ~> (char +) <~ quot
  }

  def part1 = input.map(_.length).sum - input.map(parse(escapedLine, _).get.size).sum
  def part2 = "???"
}



