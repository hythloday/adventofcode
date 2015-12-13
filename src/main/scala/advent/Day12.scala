package advent

//import play.api.libs.json._
import scala.util.parsing.json._

/**
  * Created by james on 12/12/2015.
  */
object Day12 extends Advent {

//  lazy val json: JsValue = Json.parse(input.toList.mkString)
  lazy val Some(json) = JSON.parseRaw(input.toList.mkString)

//  def count1(v: JsValue): Long = v match {
//    case JsNumber(x) => x.toLong
//    case JsArray(a) => a.map(count1).sum
//    case JsObject(o) => o.values.map(count1).sum
//    case _ => 0L
//  }

  def count(v: Any): Long = v match {
    case x: Double => x.toLong
    case JSONArray(xs) => xs.map(count).sum
    case JSONObject(m) => m.values.map(count).sum
    case _ => 0
  }

//  def count2(v: JsValue): Long = v match {
//    case JsNumber(x) => x.toLong
//    case JsArray(a) => a.map(count2).sum
//    case JsObject(o) if !(o.values.toList contains JsString("red")) => o.values.map(count2).sum
//    case _ => 0L
//  }

  def count2(v: Any): Long = v match {
    case x: Double => x.toLong
    case JSONArray(xs) => xs.map(count2).sum
    case JSONObject(m) if !(m.values.toList contains "red") => m.values.map(count2).sum
    case _ => 0
  }

  def part1 = count(json)
  def part2 = count2(json)
}
