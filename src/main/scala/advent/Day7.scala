package advent

import scala.language.postfixOps

import scala.util.parsing.combinator.JavaTokenParsers

import scala.reflect.runtime._
import scala.tools.reflect.ToolBox

/**
  * Created by james on 07/12/2015.
  */
object Day7 extends Advent with JavaTokenParsers {

  def quotedIdent = ident ^^ { case i => s"`$i`" }

  def op = {
    def expr = quotedIdent | decimalNumber
    def and = (expr <~ "AND") ~ expr ^^ { case l~r => s"$l & $r" }
    def or = (expr <~ "OR") ~ expr ^^ { case l~r => s"$l | $r" }
    def lshift = (expr <~ "LSHIFT") ~ expr ^^ { case i~l => s"$i << $l" }
    def rshift = (expr <~ "RSHIFT") ~ expr ^^ { case i~l => s"$i >> $l" }
    def not = "NOT" ~> expr ^^ { case i => s"~$i" }
    and | or | lshift | rshift | not | expr
  }

  def line = (op <~ "->") ~ quotedIdent ^^ { case o~i => s"lazy val $i = $o" }

  lazy val cm = universe.runtimeMirror(getClass.getClassLoader)
  lazy val tb = cm.mkToolBox()

  lazy val wires = parse(line +, input.mkString("\n")).get.mkString("; ")

  def part1 = tb.eval(tb.parse(s"class C { $wires }; (new C).a"))
  def part2 = tb.eval(tb.parse(s"class C { $wires }; class D extends C { override lazy val b = (new C).a }; (new D).a"))
}

