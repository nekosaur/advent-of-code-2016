import scala.collection.immutable.Queue

case class Floor(l: Int, g: Int, c: Int, b: Int) {
  def +(o: Floor) = Floor(this.l + o.l, this.g + o.g, this.c + o.c, this.b + o.b)
  def -(o: Floor) = Floor(this.l - o.l, this.g - o.g, this.c - o.c, this.b - o.b)
}
type State = List[Floor]
case class Node(state: State, moves: Int)
case class Data(queue: Queue[Node], visited: Set[State])

def check(state: State): Boolean = {
  state.foldLeft(true)((b, f) => {
    if (f.g < 0 || f.c < 0) // numbers lower than 0 are not possible
    false
    else if (f.g > 0 && f.c > f.g) // we can't have loose chips, they will be fried
    false
    else
    b && true
  })
}

def generate(state: State, visited: Set[State]): Seq[State] = {
  val actions = List(
    Floor(0, 1, 0, 1), // move 1 generator
    Floor(0, 2, 0, 1), // move 2 generators
    Floor(0, 0, 1, 1), // move 1 chip
    Floor(0, 0, 2, 1), // move 2 chips
    Floor(0, 1, 1, 1)  // move 1 generator & 1 chip
  )

  val current = state.filter(_.b == 1).head
  val generated = for {
    a <- actions
    i <- List(current.l - 1, current.l + 1).filter(x => x >= 0 && x <= 3)
  } yield {
    state.map {
      case f if f.l == current.l => f - a
      case f if f.l == i => f + a
      case f => f
    }
  }

  generated.filter(s => check(s) && !visited.contains(s))
}

def solve(start: State, goal: State): Int = {
  def run(data: Data): Int = {
    if (data.queue.nonEmpty) {
      val (node, queue) = data.queue.dequeue

      if (node.state == goal)
        return node.moves

      val states = generate(node.state, data.visited)
      val nodes = states.map(s => Node(s, node.moves + 1))

      run(Data(queue ++ nodes, data.visited ++ states))
    } else { -1 }
  }

  run(Data(Queue(Node(start, 0)), Set()))
}

val _startA = List(Floor(0, 3, 1, 1), Floor(1, 0, 2, 0), Floor(2, 2, 2, 0), Floor(3, 0, 0, 0))
val _goalA = List(Floor(0, 0, 0, 0), Floor(1, 0, 0, 0), Floor(2, 0, 0, 0), Floor(3, 5, 5, 1))

val _startB = List(Floor(0, 5, 3, 1), Floor(1, 0, 2, 0), Floor(2, 2, 2, 0), Floor(3, 0, 0, 0))
val _goalB = List(Floor(0, 0, 0, 0), Floor(1, 0, 0, 0), Floor(2, 0, 0, 0), Floor(3, 7, 7, 1))

solve(_startA, _goalA)