import io._
val input = Source.fromFile("day06.txt").getLines.toList

input.transpose.map(_.groupBy(identity).maxBy(_._2.length)._1).mkString

input.transpose.map(_.groupBy(identity).minBy(_._2.length)._1).mkString