case class Disk(n: Int, i: Int)
case class State(t: Int, d: List[Disk])

val _disksA = List(Disk(17, 15), Disk(3, 2), Disk(19, 4), Disk(13, 2), Disk(7, 2), Disk(5, 0))
val _disksB = _disksA :+ Disk(11, 0)

def check(disks: List[Disk]) = {
  def rec(xs: List[Disk]): Boolean = xs match {
    case Nil                 => true
    case y :: ys if y.i == 0 => rec(ys.map(d => Disk(d.n, (d.i + 1) % d.n)))
    case _ :: _              => false
  }

  rec(disks.map(d => Disk(d.n, (d.i + 1) % d.n)))
}

def solve(disks: List[Disk]) = {
  Stream.from(1)
        .scanLeft(State(0, disks))((s, t) => State(t, s.d.map(d => Disk(d.n, (d.i + 1) % d.n))))
        .dropWhile(s => !check(s.d))
}

solve(_disksA)
solve(_disksB)
