import io._
val input = Source.fromFile("day08.txt").getLines.toList

val (width, height) = (50, 6)
val lcd = Vector.fill(height)(Vector.fill(width)(' '))

val res = input.foldLeft(lcd)((lcd, action) => action match {
  case a if a.startsWith("rect") =>
    val w :: h :: Nil = a.split(" ")(1).split("x").toList.map(_.toInt)

    lcd.zipWithIndex.map {
      case row if row._2 < h => row._1.patch(0, List.fill(w)('X'), w)
      case row => row._1
    }

  case a if a.startsWith("rotate") =>
    val pattern = "rotate (row|column) [yx]=(\\d+) by (\\d+)".r
    val data = pattern.findFirstMatchIn(a).get
    val i = data.group(2).toInt
    val n = data.group(3).toInt

    data.group(1) match {
      case "row" =>
        lcd.zipWithIndex.map {
          case r if r._2 == i => r._1.drop(width - n) ++ r._1.take(width - n)
          case r => r._1
        }

      case "column" =>
        lcd.transpose.zipWithIndex.map {
          case c if c._2 == i => c._1.drop(height - n) ++ c._1.take(height - n)
          case c => c._1
        }.transpose
    }
})

res.flatten.count(_ == 'X')

res.foreach(r => println(r.mkString))