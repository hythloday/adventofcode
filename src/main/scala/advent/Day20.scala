package advent

/**
  * Created by james on 20/12/2015.
  */
object Day20 extends Advent {

  lazy val primes = List(2, 3, 5, 7, 11, 13, 17, 19, 23)

  def factorsOf(n: Int): Set[Int] = Set(1, n) ++ {
    for {
      i <- 2 to Math.sqrt(n.toDouble).floor.toInt
      if n % i == 0
    } yield Set(i, n/i)
  }.flatten

  def presents(n: Int) = factorsOf(n).sum * 10

  def draw(n: Int, from: List[Int]): List[Int] = if (n == 0) List(1) else for {
    h <- from
    t <- draw(n-1, from)
  } yield h * t

  def estimate(n: Int): Int = {
    val candidates = draw(n, primes.take(n)).filter(presents(_) > 36000000)
    if (candidates.nonEmpty) candidates.min else estimate(n+1)
  }

  def part1 = (1 to estimate(1)).map(n => n -> presents(n)).filter(_._2 > 36000000).minBy(_._1)._1
  def part2 = ()
}
