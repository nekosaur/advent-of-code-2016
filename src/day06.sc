import io._
val input = Source.fromFile("day06.txt").getLines.toList

input.transpose.map(l => {
  l.groupBy(_.toChar).maxBy(_._2.length)._1
}).mkString

input.transpose.map(l => {
  l.groupBy(_.toChar).minBy(_._2.length)._1
}).mkString