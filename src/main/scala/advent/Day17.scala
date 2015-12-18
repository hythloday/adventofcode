package advent

/**
  * Created by james on 17/12/2015.
  */
object Day17 extends Advent {

  lazy val buckets = input.map(_.toInt)

  def powerSet[T](elements: List[T]): Iterator[List[T]] = for {
    i <- (0 until Math.pow(2, elements.size).toInt).toIterator
  } yield elements.zip(i.toBinaryString.reverse.padTo(elements.size, '0')).filter(_._2 == '1').map(_._1)

  def part1 = powerSet(buckets).count(_.sum == 150)
  def part2 = powerSet(buckets).filter(_.sum == 150).toList.groupBy(_.size).minBy(_._1)._2.size
}
