package advent

import Math.abs

/**
  * Created by james on 05/12/2015.
  */
object Day5 extends Advent {

  def part1 = {
    def isVowel(c: Char) = "aeiou" contains c
    def prop1(s: String) = (s count isVowel) >= 3

    def isDouble(pair: String) = pair.head == pair.last
    def prop2(s: String) = s.sliding(2) exists isDouble

    def banned(pair: String) = Set("ab", "cd", "pq", "xy") contains pair
    def prop3(s: String) = !(s.sliding(2) exists banned)

    def isNice(s: String) = prop1(s) && prop2(s) && prop3(s)

    input count isNice
  }

  def part2 = {
    def positionPairs(s: String): Map[String, List[Int]] = s.sliding(2).zipWithIndex.toList.groupBy(_._1).mapValues(_.map(_._2))
    def nonAdjacentPair(pair: List[Int]) = abs(pair.head - pair.last) > 1
    def nonAdjacent(xs: List[Int]) = xs.combinations(2) exists nonAdjacentPair
    def prop1(s: String) = positionPairs(s).values exists nonAdjacent

    def repeatsWith1LetterBetween(triple: String) = triple.head == triple.last
    def prop2(s: String) = s.sliding(3) exists repeatsWith1LetterBetween

    def isNice(s: String) = prop1(s) && prop2(s)

    input count isNice
  }
}
