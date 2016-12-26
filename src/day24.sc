import io._
import scala.collection.immutable.Queue

val input = Source.fromFile("day24.txt").getLines.toList
val width = input.head.length
val height = input.length

case class Point(x: Int, y: Int) {
  def +(o: Point) = Point(this.x + o.x, this.y + o.y)
}
case class Edge(s: Point, e: Point, c: Int)

def bfs(map: Map[Point, Char], start: Point) = {
  case class State(queue: Queue[Point], edges: List[Edge], visited: Map[Point, Boolean], costs: Map[Point, Int])

  def neighbours(n: Point, map: Map[Point, Char], visited: Map[Point, Boolean]): List[Point] = {
    val directions = List(Point(-1, 0), Point(1, 0), Point(0, 1), Point(0, -1))

    directions.map(_ + n)
      .filter(p => p.x >= 0 && p.x < width && p.y >= 0 && p.y < height)
      .filter(!visited.contains(_))
      .filter(map(_) != '#')
  }

  def rec(state: State): List[Edge] = {
    if (state.queue.isEmpty) {
      state.edges
    } else {
      val (n, q) = state.queue.dequeue

      val nodes = neighbours(n, map, state.visited)
      val costs = nodes.map(p => (p, state.costs(n) + 1))
      val visited = nodes.map(p => (p, true))
      val edges = nodes.collect { case p if map(p).isDigit => Edge(start, p, state.costs(n) + 1) }

      rec(State(q.enqueue(nodes), state.edges ++ edges, state.visited ++ visited, state.costs ++ costs))
    }
  }

  rec(State(Queue(start),
    List(),
    Map[Point, Boolean]().updated(start, true).withDefaultValue(false),
    Map[Point, Int]().withDefaultValue(0)))
}

val _map = input.zipWithIndex
                .flatMap { case (r, y) => r.zipWithIndex.map { case (c, x) => (Point(x, y), c) } }
                .toMap
val _nodes = _map.collect { case (p, c) if c.isDigit => p }.toList
val _edges = _nodes.foldLeft(List[Edge]())((edges, n) => edges ++ bfs(_map, n))
val _start = _map.collect { case (p, c) if c == '0' => p }.head

def dfs(edges: Map[Point, List[Edge]], start: Point, loop: Boolean) = {
  case class State(edge: Edge, path: List[Edge])

  def rec(stack: List[State], best: Int): Int = {
    if (stack.isEmpty) {
      best
    } else {
      val curr = stack.head
      val n = edges(curr.edge.e).filter(f => !curr.path.exists(p => p.s == f.e))

      if (n.isEmpty) {
        val p = if (loop) curr.path ++ edges(curr.edge.e).filter(_.e == start) else curr.path
        val cost = p.map(_.c).sum

        rec(stack.tail, if (cost < best) cost else best)
      } else {
        val pruned = n.filter(e => (curr.path :+ e).map(_.c).sum < best)
                      .map(e => State(e, curr.path :+ e))

        rec(pruned ++ stack.tail, best)
      }

    }
  }

  rec(edges(start).map(e => State(e, List(e))), Int.MaxValue)
}

dfs(_edges.groupBy(_.s), _start, loop = false)
dfs(_edges.groupBy(_.s), _start, loop = true)
