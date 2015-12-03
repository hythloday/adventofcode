package advent

import Math._

/**
  * Created by james on 03/12/2015.
  */
object Day2 extends App {

  val input = io.Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("day2.txt")).getLines.toList

  case class Box(w: Int, h: Int, l: Int) {
    def sides = List(l*w, w*h, h*l)
    def paper: Int = sides.map(_*2).sum + sides.min
  }
  object Box {
    def apply(s: String): Box = {
      val Array(w, h, l) = s.split('x')
      Box(w.toInt, h.toInt, l.toInt)
    }
  }

  def part1 = {
    input.map(Box.apply).map(_.paper).sum
  }

  println(s"part1 = $part1")
}
