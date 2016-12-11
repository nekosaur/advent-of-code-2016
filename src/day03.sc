import io._
val fileLines = Source.fromFile("day03.txt").getLines.toList

val input = fileLines.map(l => {
  "\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)".r
    .findFirstMatchIn(l)
    .get
    .subgroups
    .map(_.toInt)
})

def check(t: List[Int]): Boolean = t(0) + t(1) > t(2) && t(1) + t(2) > t(0) && t(0) + t(2) > t(1)

input
  .count(check)

input
  .transpose
  .flatMap(_.grouped(3))
  .count(check)
