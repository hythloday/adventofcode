package advent

import scala.util.parsing.combinator.JavaTokenParsers

/**
  * Created by james on 07/12/2015.
  */
abstract class Advent extends App  with JavaTokenParsers {

  lazy val textfile = s"${getClass.getSimpleName.toLowerCase.init}.txt"
  lazy val input = io.Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(textfile)).getLines.toList

  def part1: Any
  def part2: Any

  override def main(args: Array[String]) = {
    println(s"part1 = $part1")
    println(s"part2 = $part2")
  }
}
