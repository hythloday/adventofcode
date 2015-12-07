package advent

import scala.language.postfixOps

import scala.util.parsing.combinator.JavaTokenParsers

import scala.reflect.runtime._
import scala.tools.reflect.ToolBox

/**
  * Created by james on 07/12/2015.
  */
object Day7 extends App with JavaTokenParsers {

  val input = io.Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("day7.txt")).getLines.mkString("\n")

  def quotedIdent: Parser[String] = ident ^^ { case i => s"`$i`" }

  def op = {
    def expr = quotedIdent | decimalNumber
    def and: Parser[String] = expr ~ "AND" ~ expr ^^ { case l ~ _ ~ r => s"$l & $r" }
    def or: Parser[String] = expr ~ "OR" ~ expr ^^ { case l ~ _ ~ r => s"$l | $r" }
    def lshift: Parser[String] = expr ~ "LSHIFT" ~ expr ^^ { case i ~ _ ~ l => s"$i << $l" }
    def rshift: Parser[String] = expr ~ "RSHIFT" ~ expr ^^ { case i ~ _ ~ l => s"$i >> $l" }
    def not: Parser[String] = "NOT" ~ expr ^^ { case _ ~ i => s"~$i" }
    and | or | lshift | rshift | not | expr
  }

  def line = op ~ "->" ~ quotedIdent ^^ { case o ~ _ ~ i => s"lazy val $i = $o" }

  val cm = universe.runtimeMirror(getClass.getClassLoader)
  val tb = cm.mkToolBox()

  val wires = parse(line +, input).get.mkString("; ")

  def part1 = tb.eval(tb.parse(s"class C { $wires }; (new C).a"))
  def part2 = tb.eval(tb.parse(s"class C { $wires }; class D extends C { override lazy val b = (new C).a }; (new D).a"))

  println(s"part1 = $part1")
  println(s"part2 = $part2")
}

