package advent

import advent.Day3._

/**
  * Created by james on 05/12/2015.
  */
object Day5 extends App {

  val input = io.Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("day5.txt")).getLines.toList

  def isVowel(c: Char) = "aeiou" contains c
  def prop1(s: String) = (s count isVowel) >= 3

  def isDouble(pair: String) = pair.head == pair.last
  def prop2(s: String) = s.sliding(2) exists isDouble

  def banned(pair: String) = Set("ab", "cd", "pq", "xy") contains pair
  def prop3(s: String) = !(s.sliding(2) exists banned)

  def isNice(s: String) = prop1(s) && prop2(s) && prop3(s)

  def part1 = input count isNice

  println(s"part1 = $part1")
}
