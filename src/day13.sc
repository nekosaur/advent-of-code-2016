import scala.collection.immutable.Queue
import scala.runtime.RichInt

case class Point(x: Int, y: Int) {
  def +(o: Point) = Point(this.x + o.x, this.y + o.y)
}
case class Data(maze: Map[Point, Char], visited: Set[Point], queue: Queue[Point], moves: Map[Point, Int])

def solve(start: Point, goal: Point, n: Int) = {
  def calc(p: Point) = {
    val v: RichInt = (p.x * p.x + 3 * p.x + 2 * p.x * p.y + p.y + p.y * p.y) + n
    if (v.toBinaryString.count(_ == '1') % 2 == 0) '.' else '#'
  }

  def neighbours(p: Point, visited: Set[Point], maze: Map[Point, Char]): (Map[Point, Char], List[Point]) = {
    val dirs = List(Point(-1, 0), Point(0, -1), Point(1, 0), Point(0, 1))

    val valid = dirs.map(d => p + d)
      .filter(p => p.x >= 0 && p.y >= 0)
      .filter(p => !visited.contains(p))

    val new_maze = maze ++ valid.map(p => (p, calc(p)))

    (new_maze, valid.filter(p => new_maze(p) == '.'))
  }

  def rec(data: Data): Boolean = {
    if (data.queue.nonEmpty) {
      val (p, queue) = data.queue.dequeue

      if (p == goal) {
        println("Locs in 50 moves or less = " + data.moves.count(t => t._2 <= 50))
        println("Moves to goal = " + data.moves(p))
        return true
      }

      val (maze, n) = neighbours(p, data.visited, data.maze)
      val moves = n.map(pp => (pp, data.moves(p) + 1))

      rec(Data(maze, data.visited + p, queue ++ n, data.moves ++ moves))
    } else { false }
  }

  rec(Data(Map().updated(start, '.'), Set() + start, Queue().enqueue(start), Map().updated(start, 0)))
}

solve(Point(1, 1), Point(31, 39), 1350)
