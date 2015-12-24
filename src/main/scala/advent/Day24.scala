package advent

import scala.annotation.tailrec

/**
  * Created by james on 24/12/2015.
  */
object Day24 extends Advent {

  lazy val weights = input.map(_.toLong).toSet

  @tailrec
  def smallestArrangementFor(weight: Long, choices: Set[Long], candidates: Set[Set[Long]] = Set()): Set[Set[Long]] = {
    val exact = candidates.filter(c => c.sum == weight)
    if (exact.nonEmpty) exact
    else if (candidates.isEmpty) smallestArrangementFor(weight, choices, choices.map(c => Set(c)))
    else smallestArrangementFor(weight, choices, for {
      cand <- candidates
      choice <- choices -- cand
    } yield cand + choice)
  }

  def part1 = smallestArrangementFor(weights.sum / 3, weights).toList.sortBy(_.product).dropWhile { arr =>
    smallestArrangementFor(weights.sum / 3, weights -- arr).isEmpty
  }.head.product
  def part2 = smallestArrangementFor(weights.sum / 4, weights).toList.sortBy(_.product).dropWhile { arr =>
    smallestArrangementFor(weights.sum / 4, weights -- arr).isEmpty
  }.head.product
}
