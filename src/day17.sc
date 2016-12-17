import java.security._
import scala.collection.immutable.Queue

def md5(s: String): String = {
  val hash = MessageDigest.getInstance("MD5");
  val hashed = hash.digest(s.getBytes())

  hashed.map {
    case b if (0xFF & b) < 0x10 => "0" + Integer.toHexString(0xFF & b)
    case b => Integer.toHexString(0xFF & b)
  }.mkString
}

case class Point(x: Int, y: Int) {
  def +(o: Point) = Point(this.x + o.x, this.y + o.y)
}
case class State(pos: Point, hash: String)
case class Direction(c: Char, p: Point)

def solve(input: String) = {
  def inside(p: Point) = p.x >= 0 && p.x < 4 && p.y >= 0 && p.y < 4

  def hash(s: String) = {
    md5(s).take(4).map {
      case c if c.isDigit || c == 'a' => false
      case _ => true
    }
  }

  def neighbours(state: State) = {
    val dirs = List(
      Direction('U', Point(0, -1)),
      Direction('D', Point(0, 1)),
      Direction('L', Point(-1, 0)),
      Direction('R', Point(1, 0)))

    hash(state.hash).zip(dirs).collect {
      case (b, d) if b && inside(state.pos + d.p) => State(state.pos + d.p, state.hash + d.c)
    }
  }

  def rec(queue: Queue[State], paths: List[String]): List[String] = {
    if (queue.isEmpty)
      paths
    else {
      val (s, q) = queue.dequeue

      if (s.pos == Point(3, 3))
        rec(q, paths :+ s.hash.substring(input.length))
      else
        rec(q.enqueue(neighbours(s)), paths)
    }
  }

  rec(Queue(State(Point(0, 0), input)), List())
}

val paths = solve("yjjvjgan")

paths.head
paths.last.length

