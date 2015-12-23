package advent

/**
  * Created by james on 23/12/2015.
  */
object Day23 extends Advent {

  case class Registers(a: Int, b: Int, ip: Int) {
    def nextOp = copy(ip = ip+1)
  }
  type Op = Registers => Registers

  def register = "a" | "b"
  def offset = opt("+") ~> wholeNumber
  def hlf = "hlf" ~> register ^^ {
    case "a" => (r: Registers) => r.copy(a = r.a / 2).nextOp
    case "b" => (r: Registers) => r.copy(b = r.b / 2).nextOp
  }
  def tpl = "tpl" ~> register ^^ {
    case "a" => (r: Registers) => r.copy(a = r.a * 3).nextOp
    case "b" => (r: Registers) => r.copy(b = r.b * 3).nextOp
  }
  def inc = "inc" ~> register ^^ {
    case "a" => (r: Registers) => r.copy(a = r.a + 1).nextOp
    case "b" => (r: Registers) => r.copy(b = r.b + 1).nextOp
  }
  def jmp = "jmp" ~> offset ^^ { case n => (r: Registers) => r.copy(ip = r.ip + n.toInt)}
  def jie = ("jie" ~> register) ~ ("," ~> offset) ^^ {
    case "a" ~ offset => (r: Registers) => if (r.a % 2 == 0) r.copy(ip = r.ip + offset.toInt) else r.nextOp
    case "b" ~ offset => (r: Registers) => if (r.b % 2 == 0) r.copy(ip = r.ip + offset.toInt) else r.nextOp
  }
  def jio = ("jio" ~> register) ~ ("," ~> offset) ^^ {
    case "a" ~ offset => (r: Registers) => if (r.a == 1) r.copy(ip = r.ip + offset.toInt) else r.nextOp
    case "b" ~ offset => (r: Registers) => if (r.b == 1) r.copy(ip = r.ip + offset.toInt) else r.nextOp
  }
  def op = hlf | tpl | inc | jmp | jie | jio

  lazy val ops: Array[Op] = input.map(parse(op, _).get).toArray

  def exec(r: Registers): Registers = ops(r.ip)(r)

  def execStrm(r: Registers) = {
    lazy val strm: Stream[Registers] = Stream.cons(r, strm.map(exec))
    strm.tail
  }

  def part1 = execStrm(Registers(0, 0, 0)).dropWhile(r => ops.isDefinedAt(r.ip)).head
  def part2 = execStrm(Registers(1, 0, 0)).dropWhile(r => ops.isDefinedAt(r.ip)).head
}
