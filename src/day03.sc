import io._
val fileLines = Source.fromFile("day03.txt").getLines.toList

val input = fileLines.map(l => {
  val a = l.substring(2, 5).trim.toInt
  val b = l.substring(7, 10).trim.toInt
  val c = l.substring(12, 15).trim.toInt

  List(a, b, c)
})

def check(t: List[Int]): Boolean = t(0) + t(1) <= t(2) || t(1) + t(2) <= t(0) || t(0) + t(2) <= t(1)

input
  .foldLeft(0)((c, t) => if (check(t)) c else c + 1)

input
  .transpose
  .flatMap(_.grouped(3))
  .foldLeft(0)((c, t) => if (check(t)) c else c + 1)
