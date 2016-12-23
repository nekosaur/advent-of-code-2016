import io._
import scala.collection.immutable.Queue
import scala.collection.mutable
val _input = Source.fromFile("day22.txt").getLines.toList

case class Point(x: Int, y: Int) {
  def +(o: Point) = Point(this.x + o.x, this.y + o.y)
}
case class Disk(loc: Point, size: Int, used: Int, free: Int, goal: Boolean)

val Pattern = "/dev/grid/node-x(\\d+)-y(\\d+)\\s+(\\d+)T\\s+(\\d+)T\\s+(\\d+)T.*".r

val _nodes = _input.collect {
  case Pattern(x, y, size, used, free) => Disk(Point(x.toInt, y.toInt), size.toInt, used.toInt, free.toInt, false)
}

def solveA(nodes: List[Disk]) = {
  nodes.flatMap(a => {
    nodes.filter(!_.equals(a))
      .collect { case b if a.used > 0 && a.used <= b.free => (a, b) }
  }).length
}

solveA(_nodes)

case class Node(current: Point, goal: Point) {
  def weight = math.abs(current.y - goal.y) + math.abs(current.x - goal.x) + goal.x + goal.y
}
case class State(queue: mutable.PriorityQueue[Node], visited: Set[Node], costs: Map[Node, Int], parents: Map[Node, Node])

val _width = 36
val _height = 30
val _map = _nodes.map { case n if n.size > 100 => (n.loc, false); case n => (n.loc, true) }.toMap
val _start = _nodes.filter(_.used == 0).head.loc
val _goal = Point(_width - 1, 0)

def solveB(map: Map[Point, Boolean], start: Point, goal: Point) = {
  def rec(state: State): Int = {
    if (state.queue.isEmpty) {
      -1
    } else {

    }
  }

  rec(State(mutable.PriorityQueue(Node(start, goal)), Set(), Map().withDefaultValue(Int.MaxValue), Map().withDefaultValue(null)))
}
val queue = mutable.PriorityQueue[Node]()(Ordering.by(_.weight))

