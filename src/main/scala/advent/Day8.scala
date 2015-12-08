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
  def hex = "[0-9a-f]".r
  def escChr = bs ~ "x" ~ hex ~ hex
  def chr = "[a-z]".r
  def char = escBs | escQuot | escChr | chr
  def escapedLine = quot ~> (char +) <~ quot

  def part1 = input.map(_.length).sum - input.map(parse(escapedLine, _).get.size).sum

  def quotLen = quot ^^^ { 2 }
  def bsLen = bs ^^^ { 2 }
  def escQuotLen = escQuot ^^^ { 4 }
  def escChrLen = escChr ^^^ { 5 }
  def charLen = chr ^^^ { 1 }
  def lineLen = quotLen | escChrLen | charLen | bsLen | escQuotLen
  def escape(line: String) = parse(lineLen +, line).get.sum + 2

  def part2 = input.map(escape).sum - input.map(_.length).sum
}



