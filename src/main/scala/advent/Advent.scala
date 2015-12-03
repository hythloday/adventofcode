package advent


/**
  * Created by james on 03/12/2015.
  */
object Advent extends App {

  def day1: Int = {
    val input = io.Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("day1.txt")).getLines.mkString
    val (up, down) = input.partition(_ == '(')
    up.length - down.length
  }

  println(s"day1 = $day1")
}
