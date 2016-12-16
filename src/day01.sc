import io._
val input = Source.fromFile("day01.txt").getLines.toList.head.split(", ").toList

object Direction {
  sealed abstract class Enum(val n: Int) {
    def left = if (this.n == 0) Direction.DOWN else Direction.values(n - 1)
    def right = if (this.n == 3) Direction.LEFT else Direction.values(n + 1)
  }
  case object LEFT extends Enum(0)
  case object UP extends Enum(1)
  case object RIGHT extends Enum(2)
  case object DOWN extends Enum(3)
  val values = Seq(LEFT, UP, RIGHT, DOWN)
}

case class Point(x: Int, y: Int)
case class Line(s: Point, e: Point)
case class Data(p: Point, l: List[Line], d: Direction.Enum)
case class Action(c: Char, n: Int)

def cross(a: Point, b: Point) = a.x * b.y - a.y * b.x

def isLeftTurn(p: Point, q: Point, r: Point): Boolean = {
  val u = Point(q.x - p.x, q.y - p.y)
  val v = Point(r.x - q.x, r.y - q.y)
  val m = cross(u, v)
  if (m < 0) true else false
}

def intersect(a: Line, b: Line) = {
  if (a.e != b.s && a.s != b.e) {
    val first = isLeftTurn(a.s, a.e, b.s)
    if (isLeftTurn(a.s, a.e, b.e) != first) {
      val second = isLeftTurn(b.s, b.e, a.s)
      if (isLeftTurn(b.s, b.e, a.e) != second) true else false
    } else { false }
  } else { false }
}

def check(lines: List[Line], line: Line): Option[Point] = lines match {
  case Nil => None
  case y :: _ if intersect(y, line) =>
    if (y.s.x == y.e.x) Option(Point(y.s.x, line.s.y)) else Option(Point(y.s.y, line.s.x))
  case _ :: ys => check(ys, line)
}

val actions = input.map(s => Action(s(0), s.substring(1).toInt))
val start = Data(Point(0, 0), List(), Direction.UP)

val res = actions.foldLeft(start)((d, a) => {
  val dir = if (a.c == 'R') d.d.right else d.d.left

  val pos = dir match {
    case Direction.UP    => Point(d.p.x, d.p.y + a.n)
    case Direction.DOWN  => Point(d.p.x, d.p.y - a.n)
    case Direction.LEFT  => Point(d.p.x - a.n, d.p.y)
    case Direction.RIGHT => Point(d.p.x + a.n, d.p.y)
  }

  val l = Line(d.p, pos)
  val p = check(d.l, l)

  if (p.isDefined) println(Math.abs(p.get.x) + Math.abs(p.get.y))

  Data(pos, d.l :+ l, dir)
})

Math.abs(res.p.x) + Math.abs(res.p.y)