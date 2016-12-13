import io._
import scala.annotation.tailrec
val input = Source.fromFile("day12.txt").getLines.toList

case class Computer(registers: Map[String, Int], i: Int, instructions: List[String])

val _registersA = Map[String, Int]("a" -> 0, "b" -> 0, "c" -> 0, "d" -> 0)
val _registersB = Map[String, Int]("a" -> 0, "b" -> 0, "c" -> 1, "d" -> 0)

def solve(registers: Map[String, Int], instructions: List[String]) = {
  @tailrec
  def rec(c: Computer): Computer = c.i match {
    case i if i >= c.instructions.length =>
      c

    case i =>
      val instr :: data = c.instructions(c.i).split(" ").toList
      instr match {
        case "cpy" =>
          val x :: y :: Nil = data
          val v = if (c.registers.contains(x)) c.registers(x) else x.toInt
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
          val v = if (j != 0) y.toInt else 1
          rec(Computer(c.registers, c.i + v, c.instructions))
      }
  }

  rec(Computer(registers, 0, instructions))
}

solve(_registersA, input)

solve(_registersB, input)