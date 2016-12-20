import io._
val input = Source.fromFile("day09.txt").getLines.toList.head

def decompress(recurse: Boolean)(xs: String) = {
  def rec(xs: Vector[Char], length: BigInt): BigInt = xs match {
    case Seq() => length
    case ys if ys(0) == '(' =>
      val i = ys.indexOf(')')
      val chars :: repeat :: Nil = ys.slice(1, i).mkString.split("x").map(_.toInt).toList
      if (recurse)
        rec(ys.drop(i + 1 + chars), length) + repeat * rec(ys.slice(i + 1, i + 1 + chars), 0)
      else
        rec(ys.drop(i + 1 + chars), length + (chars * repeat))
    case ys => rec(ys.drop(1), length + 1)
  }

  rec(xs.toVector, BigInt.apply(0))
}

decompress(false)(input)
decompress(true)(input)
