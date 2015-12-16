package advent

/**
  * Created by james on 16/12/2015.
  */
object Day16 extends Advent {

  type Sue = Map[String, Int]

  def property = (ident <~ ":") ~ wholeNumber ^^ { case n~q => n -> q.toInt }
  def aSue = ("Sue" ~> wholeNumber <~ ":") ~ repsep(property, ",") ^^ { case n~things => n -> things.toMap }

  lazy val sues: Map[String, Map[String, Int]] = input.map(parse(aSue, _).get).toMap

  lazy val sender = parse(rep(property),
    io.Source.fromURL(getClass.getClassLoader.getResource("day16-sender.txt")).mkString).get.toMap

  def canBeSender(sender: Sue)(b: Sue) = sender.filter{ case (k, v) => b.get(k).isDefined } == b

  def canBeSenderPart2(sender: Sue)(b: Sue) = sender.keys.filter(b.contains).map(k => (k, sender(k), b(k))).forall {
    case (thing, sv, bv) if Set("cats", "trees") contains thing           => bv > sv
    case (thing, sv, bv) if Set("pomeranians", "goldfish") contains thing => bv < sv
    case (_, sv, bv)                                                      => sv == bv
  }

  def part1 = sues.collect{ case (n, sue) if canBeSender(sender)(sue) => n }
  def part2 = sues.collect{ case (n, sue) if canBeSenderPart2(sender)(sue) => n }
}
