import io._
val _input = Source.fromFile("day21.txt").getLines.toList

def rotateLeft(xs: Vector[Char], n: Int, reverse: Boolean): Vector[Char] = {
  if (n % xs.length == 0) xs
  else if (reverse) rotateRight(xs, n, false)
  else xs.drop(n % xs.length) ++ xs.take(n % xs.length)
}

def rotateRight(xs: Vector[Char], n: Int, reverse: Boolean): Vector[Char] = {
  if (n % xs.length == 0) xs
  else if (reverse) rotateLeft(xs, n, false)
  else xs.takeRight(n % xs.length) ++ xs.dropRight(n % xs.length)
}

def swap_position(v: Vector[Char], x: Int, y: Int) = v.updated(x, v(y)).updated(y, v(x))

def swap_char(v: Vector[Char], x: Char, y: Char) = {
  val ix = v.indexOf(x)
  val iy = v.indexOf(y)
  v.updated(ix, v(iy)).updated(iy, v(ix))
}

def rotate_char(v: Vector[Char], x: Char) = v.indexOf(x) match {
  case i if i >= 4 => rotateRight(v, 1 + i + 1, false)
  case i => rotateRight(v, 1 + i, false)
}

def rotate_char_r(v: Vector[Char], x: Char) = Stream.continually(1)
                                                    .scanLeft(v)((vv, n) => rotateLeft(vv, n, false))
                                                    .dropWhile(!rotate_char(_, x).equals(v))
                                                    .head

def reverse_slice(v: Vector[Char], x: Int, y: Int) = v.patch(x, v.slice(x, y + 1).reverse, y - x + 1)

def move(v: Vector[Char], x: Int, y: Int) = {
  val vv = v.patch(x, Nil, 1)
  vv.take(y) ++ Vector(v(x)) ++ vv.drop(y)
}

val SwapPos = "^swap position (\\d+) with position (\\d+)".r
val SwapChar = "^swap letter (\\w) with letter (\\w)".r
val RotateChar = "^rotate based on position of letter (\\w)".r
val RotateDir = "^rotate (\\w+) (\\d+) step[s]?".r
val ReversePos = "^reverse positions (\\d+) through (\\d+)".r
val MovePos = "^move position (\\d+) to position (\\d+)".r

def solve(input: String, actions: List[String], reverse: Boolean) = {
  val ordered = if (reverse) actions.reverse else actions
  ordered.foldLeft(input.toVector) {
    case (v, SwapPos(x, y)) => swap_position(v, x.toInt, y.toInt)
    case (v, SwapChar(x, y)) => swap_char(v, x.head, y.head)
    case (v, RotateChar(x)) => if (reverse) rotate_char_r(v, x.head) else rotate_char(v, x.head)
    case (v, RotateDir(dir, x)) if dir == "left" => rotateLeft(v, x.toInt, reverse)
    case (v, RotateDir(dir, x)) if dir == "right" => rotateRight(v, x.toInt, reverse)
    case (v, ReversePos(x, y)) => reverse_slice(v, x.toInt, y.toInt)
    case (v, MovePos(x, y)) => if (reverse) move(v, y.toInt, x.toInt) else move(v, x.toInt, y.toInt)
  }.mkString
}

solve("abcdefgh", _input, reverse = false)
solve("fbgdceah", _input, reverse = true)
