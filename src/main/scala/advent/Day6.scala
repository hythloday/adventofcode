package advent

import scala.util.parsing.combinator._
import Math.max

/**
  * Created by james on 06/12/2015.
  */
object Day6 extends Advent {

  object Op extends Enumeration {
    type Op = Value
    val Toggle, On, Off = Value
  }
  import Op._
  type Coord = (Int, Int)

  case class Mutate(op: Op, topLeft: Coord, bottomRight: Coord) {
    def contains(c: Coord) = c match {
      case (x, _) if x < topLeft._1 || x > bottomRight._1 => false
      case (_, y) if y < topLeft._2 || y > bottomRight._2 => false
      case _ => true
    }
  }

  def op = {
    def toggle: Parser[Op] = "toggle" ^^^ { Toggle }
    def turnOn: Parser[Op] = "turn on" ^^^ { On }
    def turnOff: Parser[Op] = "turn off" ^^^ { Off }
    toggle | turnOn | turnOff
  }

  def coord: Parser[(Int, Int)] = wholeNumber ~ "," ~ wholeNumber ^^ { case x ~ _ ~ y => (x.toInt, y.toInt) }

  def line = op ~ coord ~ "through" ~ coord ^^ { case op ~ tl ~ _ ~ br => Mutate(op, tl, br) }

  case class Display(history: List[Mutate]) {
    private val r = history.reverse

    def lightOnAt(c: Coord) = {
      def intersect(h: List[Mutate]): Boolean = h match {
        case Nil => false
        case Mutate(On, _, _) :: tail if h.head.contains(c) => true
        case Mutate(Off, _, _) ::tail if h.head.contains(c) => false
        case Mutate(Toggle, _, _) :: tail if h.head.contains(c) => !intersect(tail)
        case nop :: tail => intersect(tail)
      }
      intersect(r)
    }

    def lightBrightnessAt(c: Coord) = history.foldLeft(0) { (brightness, mut) => mut.op match {
      case On if mut.contains(c) => brightness + 1
      case Off if mut.contains(c) => max(0, brightness - 1)
      case Toggle if mut.contains(c) => brightness + 2
      case _ => brightness
    }}
  }

  lazy val d = Display(input.map(parse(line, _).get))

  def part1 = {
    for {
      x <- 0 to 999
      y <- 0 to 999
      if d.lightOnAt((x, y))
    } yield true
  }.size

  def part2 = {
    for {
      x <- 0 to 999
      y <- 0 to 999
    } yield d.lightBrightnessAt((x, y))
  }.sum
}
