import io._
val _input = Source.fromFile("day20.txt").getLines.toList

case class Blacklist(from: BigInt, to: BigInt)
case class State(c: Blacklist, l: List[BigInt])

val _list = _input.map(l => {
  val from :: to :: Nil = l.split("-").map(BigInt(_)).toList
  Blacklist(from, to)
})

def solveA(list: List[Blacklist]) = {
  val res = list.sortBy(_.from).foldLeft(Blacklist(0, 0)) {
    case (c, b) if b.to < c.to => c
    case (c, b) if b.from - c.to <= 1 && b.to > c.to => Blacklist(c.from, b.to)
    case (c, _) => c
  }

  res.to + 1
}

def solveB(list: List[Blacklist]) = {
  val res = list.sortBy(_.from).foldLeft(State(Blacklist(0, 0), List[BigInt]())) {
    case (s, b) if b.to < s.c.to => s
    case (s, b) if b.from - s.c.to <= 1 && b.to > s.c.to => State(Blacklist(s.c.from, b.to), s.l)
    case (s, b) => State(b, s.l :+ (b.from - s.c.to - 1))
  }

  res.l.sum
}

solveA(_list)
solveB(_list)
