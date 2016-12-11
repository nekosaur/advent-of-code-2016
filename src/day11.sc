case class Floor(g: Int, c: Int, b: Int) {
  def +(o: Floor) = Floor(this.g + o.g, this.c + o.c, this.b + o.b)
  def -(o: Floor) = Floor(this.g - o.g, this.c - o.c, this.b - o.b)
}
type State = List[Floor]
case class Node(state: State, moves: Int, visited: List[State])

def check(state: State): Boolean = {
  state.foldLeft(true)((b, f) => {
    if (f.g < 0 || f.c < 0)
      false
    else if (f.g > 0 && f.c > f.g)
      false
    else
      b && true
  })
}

def generate(state: State, visited: Set[State]): Seq[State] = {
  def actions = List(
    Floor(1, 0, 1),
    Floor(2, 0, 1),
    Floor(0, 1, 1),
    Floor(0, 2, 1),
    Floor(1, 1, 1)
  )

  val current = state.zipWithIndex.filter(_._1.b == 1).head
  val generated = for {
    a <- actions
    i <- Range(current._2 - 1, current._2 + 2).filter(i => i >= 0 && i <= 3 && i != current._2.floor)
  } yield {
    state.zipWithIndex.map(t => {
      if (t._2 == current._2)
        t._1 - a
      else if (t._2 == i)
        t._1 + a
      else
        t._1
    })
  }

  generated.filter(s => check(s) && !visited.contains(s))
}

def solve(start: State, goal: State): Int = {
  val queue = scala.collection.mutable.Queue[Node](Node(start, 0, List[State]()))
  var visited = Set[State]()

  while (queue.nonEmpty) {
    val node = queue.dequeue()

    if (node.state == goal)
      return node.moves

    generate(node.state, visited).foreach(s => {
      visited = visited + s
      queue.enqueue(Node(s, node.moves + 1, Nil))
    })
  }

  return -1
}

val _startA = List(Floor(3, 1, 1), Floor(0, 2, 0), Floor(2, 2, 0), Floor(0, 0, 0))
val _goalA = List(Floor(0, 0, 0), Floor(0, 0, 0), Floor(0, 0, 0), Floor(5, 5, 1))

val _startB = List(Floor(5, 3, 1), Floor(0, 2, 0), Floor(2, 2, 0), Floor(0, 0, 0))
val _goalB = List(Floor(0, 0, 0), Floor(0, 0, 0), Floor(0, 0, 0), Floor(7, 7, 1))

solve(_startB, _goalB)