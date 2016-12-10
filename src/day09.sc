import io._
val input = Source.fromFile("day09.txt").getLines.toList.head

def decompress(recurse: Boolean)(xs: String) = {
  def rec(xs: List[Char], length: BigInt): BigInt = xs match {
    case Nil => length
    case y :: ys if y == '(' =>
      val i = ys.indexOf(')')
      val chars :: repeat :: Nil = ys.take(i).mkString.split("x").map(_.toInt).toList
      if (recurse)
        rec(ys.drop(i + 1 + chars), length) + repeat * rec(ys.drop(i + 1).take(chars), 0)
      else
        rec(ys.drop(i + 1 + chars), length + (chars * repeat))
    case _ :: ys => rec(ys, length + 1)
  }

  rec(xs.toList, BigInt.apply(0))
}

decompress(false)(input)

decompress(true)(input)