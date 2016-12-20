var _input = 3012210

case class Elf(i: Int, n: Int)

def solveA(n: Int) = {
  def iter(elfs: Vector[Elf]) = elfs.sliding(2, 2).collect {
    case Seq(x, y) => Elf(x.i, x.n + y.n)
  }.toVector

  def rec(elfs: Vector[Elf]): Int = {
    if (elfs.length == 1)
      elfs(0).i
    else if (elfs.length % 2 == 0) {
      rec(iter(elfs))
    } else {
      val even = iter(elfs.init)
      rec(even.tail :+ Elf(elfs.last.i, elfs.last.n + even.head.n))
    }
  }

  rec(Stream.from(1).map(Elf(_, 1)).take(n).toVector)
}

solveA(_input)

// Not really happy with this solution

case class Node(i: Int) {
  var prev: Node = null
  var next: Node = null
  def walk(n: Int): Node = {
    var curr = this
    var c = n
    while (c > 0) {
      curr = curr.next
      c -= 1
    }
    curr
  }
}

def solveB(elves: Int) = {
  val start = Node(1)
  var current = start

  Range(2, elves + 1).foreach(i => {
    val n = Node(i)
    current.next = n
    n.prev = current
    current = n
  })

  current.next = start
  start.prev = current

  var curr = start.prev
  var elvesLeft = elves
  var offset = if (elves % 2 == 0) 2 else 1
  var opp = start.walk((elves / 2) - offset)

  while (elvesLeft > 0) {
    curr = curr.next
    offset = if (elvesLeft % 2 == 0) 2 else 1
    opp = opp.walk(offset)

    opp.next.prev = opp.prev
    opp.prev.next = opp.next

    elvesLeft -= 1
  }

  curr.i
}

solveB(_input)