package advent

/**
  * Created by james on 11/12/2015.
  */
object Day11 extends Advent {

  def increasingStraight(s: String) = (s.head+1).toChar == s.charAt(1) && s.charAt(1) == (s.last-1).toChar
  def pred1(s: String) = s.sliding(3) exists increasingStraight

  def pred2(s: String) = !((s contains 'i') || (s contains 'o') || (s contains 'l'))

  def pairOf(s: String) = if (s.head == s.last) Some(s.head.toString) else None
  def pred3(s: String) = (s.sliding(2) flatMap pairOf).toList.distinct.size > 1

  def next(s: String) = s.foldRight("" -> 1) {
    case ('z', (accum, 1)) => "a" + accum -> 1
    case (c, (accum, carry)) => ((c+carry).toChar.toString + accum) -> 0
  }._1
  def passwordStream(s: String) = {
    lazy val strm: Stream[String] = Stream.cons(s, strm.map(next))
    strm.tail
  }

  def part1 = passwordStream("cqjxjnds") filter pred1 filter pred2 filter pred3 head
  def part2 = passwordStream(part1) filter pred1 filter pred2 filter pred3 head

}
