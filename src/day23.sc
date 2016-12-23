import io._
import scala.annotation.tailrec
val input = Source.fromFile("day23.txt").getLines.toList

case class Computer(registers: Map[String, Int], i: Int, instructions: List[String])

val _registersA = Map[String, Int]("a" -> 7, "b" -> 0, "c" -> 0, "d" -> 0)
val _registersB = Map[String, Int]("a" -> 12, "b" -> 0, "c" -> 0, "d" -> 0)

def toggle(instructions: List[String], i: Int) = {
  if (i < 0 || i >= instructions.length) {
    instructions
  } else {
    val instr :: data = instructions(i).split(" ").toList
    var new_instr = ""

    if (data.length > 1)
      new_instr = if (instr == "jnz") "cpy" else "jnz"
    else
      new_instr = if (instr == "inc") "dec" else "inc"

    instructions.updated(i, (new_instr +: data).mkString(" "))
  }
}

def check_multiply(instructions: List[String], i: Int) = {
  if (i + 5 >= instructions.length)
    false
  else {
    instructions.slice(i, i + 5).map(_.split(" ")(0)) == List("inc", "dec", "jnz", "dec", "jnz")
  }
}

def multiply(registers: Map[String, Int], instructions: List[String], i: Int) = {
    val t = instructions(i).split(" ")(1)
    val a = instructions(i + 1).split(" ")(1)
    val b = instructions(i + 3).split(" ")(1)
    val av = registers(a)
    val bv = registers(b)

    registers.updated(t, registers(t) + (av * bv)).updated(a, 0).updated(b, 0)
}

def solve(registers: Map[String, Int], instructions: List[String]) = {
  @tailrec
  def rec(c: Computer): Computer = {
    if (c.i >= c.instructions.length) {
      c
    } else {
      if (check_multiply(c.instructions, c.i)) {
        val regs = multiply(c.registers, c.instructions, c.i)
        rec(Computer(regs, c.i + 5, c.instructions))
      } else {

        val instr :: data = c.instructions(c.i).split(" ").toList
        instr match {
          case "tgl" =>
            val x :: Nil = data
            val v = if (c.registers.contains(x)) c.registers(x) else x.toInt

            rec(Computer(c.registers, c.i + 1, toggle(c.instructions, c.i + v)))
          case "cpy" =>
            val x :: y :: Nil = data
            val v = if (c.registers.contains(x)) c.registers(x) else x.toInt

            if (!c.registers.contains(y))
              rec(c.copy(i = c.i + 1))
            else
              rec(Computer(c.registers.updated(y, v), c.i + 1, c.instructions))

          case "inc" =>
            val x :: Nil = data
            val v = c.registers(x)
            rec(Computer(c.registers.updated(x, v + 1), c.i + 1, c.instructions))

          case "dec" =>
            val x :: Nil = data
            val v = c.registers(x)
            rec(Computer(c.registers.updated(x, v - 1), c.i + 1, c.instructions))

          case "jnz" =>
            val x :: y :: Nil = data
            val j = if (c.registers.contains(x)) c.registers(x) else x.toInt
            val v = if (j != 0) if (c.registers.contains(y)) c.registers(y) else y.toInt else 1
            rec(Computer(c.registers, c.i + v, c.instructions))
        }
      }
    }
  }

  rec(Computer(registers, 0, instructions))
}

solve(_registersA, input)
solve(_registersB, input)
