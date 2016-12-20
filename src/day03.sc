import io._
val fileLines = Source.fromFile("day03.txt").getLines.toList

val input = fileLines.map(l => {
  "\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)".r
    .findFirstMatchIn(l)
    .map(m => m.subgroups.map(_.toInt).toVector)
})

def check(t: Vector[Int]): Boolean = t(0) + t(1) > t(2) && t(1) + t(2) > t(0) && t(0) + t(2) > t(1)

input.count { case Some(x) => check(x); case None => false }

input
  .transpose { case Some(x) => x; case None => List() }
  .flatMap(_.grouped(3))
  .map(_.toVector)
  .count(check)
