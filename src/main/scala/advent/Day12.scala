package advent

import play.api.libs.json._

/**
  * Created by james on 12/12/2015.
  */
object Day12 extends Advent {

  lazy val json: JsValue = Json.parse(input.mkString)

  def count1(v: JsValue): Long = v match {
    case JsNumber(x) => x.toLong
    case JsArray(a) => a.map(count1).sum
    case JsObject(o) => o.values.map(count1).sum
    case _ => 0L
  }

  def count2(v: JsValue): Long = v match {
    case JsNumber(x) => x.toLong
    case JsArray(a) => a.map(count2).sum
    case JsObject(o) if !(o.values.toList contains JsString("red")) => o.values.map(count2).sum
    case _ => 0L
  }

  def part1 = count1(json)
  def part2 = count2(json)
}
