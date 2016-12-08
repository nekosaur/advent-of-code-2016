import io._
val input = Source.fromFile("day08.txt").getLines.toList

val lcd = Array.fill(6)(Array.fill(50)(' '))

val res = input.foldLeft(lcd)((lcd, action) => action match {
  case a if a.startsWith("rect") =>
    val width :: height :: Nil = a.split(" ")(1).split("x").toList.map(_.toInt)

    for (y <- 0 until height) {
      for (x <- 0 until width) {
        lcd(y)(x) = 'X'
      }
    }

    lcd

  case a if a.startsWith("rotate row") =>
    val pattern = "rotate row y=(\\d+) by (\\d+)".r
    val data = pattern.findFirstMatchIn(a).get
    val y :: n :: Nil = data.subgroups.map(_.toInt)

    val row = lcd(y).clone
    val indices = Range(n, 50) ++ Range(0, n)
    var i = 0
    for (x <- indices) {
      lcd(y)(x) = row(i)
      i = i + 1
    }

    lcd

  case a if a.startsWith("rotate column") =>
    val pattern = "rotate column x=(\\d+) by (\\d+)".r
    val data = pattern.findFirstMatchIn(a).get
    val x :: n :: Nil = data.subgroups.map(_.toInt)

    val row = lcd.transpose.toList(x)
    val indices = Range(n, 6) ++ Range(0, n)
    var i = 0
    for (y <- indices) {
      lcd(y)(x) = row(i)
      i = i + 1
    }

    lcd
})

res.flatMap(r => r.toList).toList.count(x => x == 'X')

res.foreach(r => println(r.mkString))