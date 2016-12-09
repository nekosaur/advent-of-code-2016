import io._
val input = Source.fromFile("day09.txt").getLines.toList.head

def decompressA(xs: String) = {
  def rec(xs: List[Char], length: BigInt): BigInt = xs match {
    case Nil => length
    case y :: ys if y == '(' =>
      val i = ys.indexOf(')')
      val chars :: repeat :: Nil = ys.take(i).mkString.split("x").map(_.toInt).toList
      rec(ys.drop(i + 1 + chars), length + (chars * repeat))
    case _ :: ys => rec(ys, length + 1)
  }

  rec(xs.toList, BigInt.apply(0))
}

decompressA(input)

def decompressB(xs: String) = {
  def rec(xs: List[Char], length: BigInt): BigInt = xs match {
    case Nil => length
    case y :: ys if y == '(' =>
      val i = ys.indexOf(')')
      val chars :: repeat :: Nil = ys.take(i).mkString.split("x").map(_.toInt).toList
      rec(ys.drop(i + 1 + chars), length) + repeat * rec(ys.drop(i + 1).take(chars), 0)
    case _ :: ys => rec(ys, length + 1)
  }

  rec(xs.toList, BigInt.apply(0))
}

decompressB(input)