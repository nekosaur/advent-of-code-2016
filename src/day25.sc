import io._
import scala.annotation.tailrec
val input = Source.fromFile("day25.txt").getLines.toList

case class Computer(registers: Map[String, Int], i: Int, instructions: List[String], out: List[Int])

val _registersA = Map[String, Int]("a" -> 0, "b" -> 0, "c" -> 0, "d" -> 0)

def toggle(c: Computer, r: String) = {
  val i = c.i + c.registers(r)
  if (i < 0 || i >= c.instructions.length) {
    c.instructions
  } else {
    val instr :: data = c.instructions(i).split(" ").toList
    var new_instr = ""

    if (data.length > 1)
      new_instr = if (instr == "jnz") "cpy" else "jnz"
    else
      new_instr = if (instr == "inc") "dec" else "inc"

    c.instructions.updated(i, (new_instr +: data).mkString(" "))
  }
}

def check_multiply(c: Computer) = {
  if (c.i + 5 >= c.instructions.length)
    false
  else {
    val sig = List("inc", "dec", "jnz", "dec", "jnz")
    c.instructions.slice(c.i, c.i + 5).map(_.split(" ")(0)) == sig
  }
}

def multiply(c: Computer) = {
  val t = c.instructions(c.i).split(" ")(1)
  val a = c.instructions(c.i + 1).split(" ")(1)
  val b = c.instructions(c.i + 3).split(" ")(1)
  val av = c.registers(a)
  val bv = c.registers(b)

  val r = c.registers
           .updated(t, c.registers(t) + (av * bv))
           .updated(a, 0)
           .updated(b, 0)

  c.copy(registers = r, i = c.i + 5)
}

def solve(registers: Map[String, Int], instructions: List[String]) = {
  val Toggle = "tgl (\\S)".r
  val Copy = "cpy (\\S+) (\\S)".r
  val Inc = "inc (\\S)".r
  val Dec = "dec (\\S)".r
  val Jnz = "jnz (\\S) (\\S+)".r
  val Out = "out (\\S)".r

  def inc(r: Map[String, Int], x: String) = r.updated(x, r(x) + 1)
  def dec(r: Map[String, Int], x: String) = r.updated(x, r(x) - 1)

  @tailrec
  def rec(c: Computer): Computer = {
    if (c.i >= c.instructions.length) {
      c
    } else {
      c.instructions(c.i) match {
        case _ if check_multiply(c) => rec(multiply(c))
        case Toggle(x) => rec(c.copy(i = c.i + 1, instructions = toggle(c, x)))
        case Copy(x, y) =>
          val v = if (c.registers.contains(x)) c.registers(x) else x.toInt
          val regs = if (c.registers.contains(y)) c.registers.updated(y, v) else c.registers
          rec(Computer(regs, c.i + 1, c.instructions, c.out))
        case Inc(x) => rec(c.copy(registers = inc(c.registers, x), i = c.i + 1))
        case Dec(x) => rec(c.copy(registers = dec(c.registers, x), i = c.i + 1))
        case Jnz(x, y) =>
          val j = if (c.registers.contains(x)) c.registers(x) else x.toInt
          val v = if (j != 0) if (c.registers.contains(y)) c.registers(y) else y.toInt else 1
          rec(Computer(c.registers, c.i + v, c.instructions, c.out))
        case Out(x) =>
          val v = c.registers(x)
          if (c.out.nonEmpty && c.out.head == v) {
            throw new RuntimeException("clock error!")
          }
          rec(c.copy(i = c.i + 1, out = v :: c.out))
      }
    }
  }

  rec(Computer(registers, 0, instructions, Nil))
}

Stream.from(0).foreach(i => {
  try {
    println("trying " + i)
    solve(_registersA.updated("a", i), input)
  } catch {
    case _: RuntimeException => None
  }
})
