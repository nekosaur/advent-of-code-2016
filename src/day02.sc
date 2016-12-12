import io._
val input = Source.fromFile("day02.txt").getLines.toList

val numpad = List(
  List[Int](1, 2, 3),
  List[Int](4, 5, 6),
  List[Int](7, 8, 9)
)

case class Point(x: Int, y: Int)
case class Data(p: Point, pwd: String)

input.foldLeft(Data(Point(1, 1), ""))((d, r) => {
  val pos = r.foldLeft(d.p)((p, c) => c match {
    case 'L' if p.x > 0 => Point(p.x - 1, p.y)
    case 'U' if p.y > 0 => Point(p.x, p.y - 1)
    case 'R' if p.x < 2 => Point(p.x + 1, p.y)
    case 'D' if p.y < 2 => Point(p.x, p.y + 1)
    case _ => p
  })

  Data(pos, d.pwd + numpad(pos.y)(pos.x))
}).pwd

val extreme_numpad = List(
  List[Char]('X', 'X', '1', 'X', 'X'),
  List[Char]('X', '2', '3', '4', 'X'),
  List[Char]('5', '6', '7', '8', '9'),
  List[Char]('X', 'A', 'B', 'C', 'X'),
  List[Char]('X', 'X', 'D', 'X', 'X')
)

def valid(x: Int, y: Int): Boolean = extreme_numpad(y)(x) != 'X'

input.foldLeft(Data(Point(1, 1), ""))((d, r) => {
  val pos = r.foldLeft(d.p)((p, c) => c match {
    case 'L' if p.x > 0 && valid(p.x - 1, p.y) => Point(p.x - 1, p.y)
    case 'U' if p.y > 0 && valid(p.x, p.y - 1) => Point(p.x, p.y - 1)
    case 'R' if p.x < 4 && valid(p.x + 1, p.y) => Point(p.x + 1, p.y)
    case 'D' if p.y < 4 && valid(p.x, p.y + 1) => Point(p.x, p.y + 1)
    case _ => p
  })

  Data(pos, d.pwd + extreme_numpad(pos.y)(pos.x))
}).pwd