import io._
val fileLines = Source.fromFile("day04.txt").getLines.toList

case class Room(name: String, sector: Int, checksum: String)

val rooms = fileLines.map(l => {
  val pattern = "(.+)-(\\d+)\\[(\\w+)\\]$".r
  val m = pattern.findAllIn(l).matchData.next()

  Room(m.group(1), m.group(2).toInt, m.group(3))
})

val validRooms = rooms.filter(r => {
  r.name.replace("-", "")
    .groupBy(_.toChar)
    .mapValues(_.length)
    .toList
    .sortWith((p1, p2) => {
      if (p1._2 > p2._2)
        true
      else if (p1._2 < p2._2)
        false
      else {
        if (p1._1 < p2._1) true else false
      }
    })
    .take(5)
    .map(_._1)
    .zip(r.checksum)
    .foldLeft(true)((v, p) => {
      if (p._1 != p._2) false else v && true
    })
})

// sector sum of all valid rooms
validRooms.map(r => r.sector).sum

// decode valid room names, filter out room with north in name
validRooms.map(r => {
  val alphabet = "abcdefghijklmnopqrstuvwxyz"
  val dec = r.name.map(c => {
    if (c == '-') ' ' else alphabet((alphabet.indexOf(c) + r.sector) % alphabet.length)
  })

  Room(dec, r.sector, r.checksum)
}).filter(p => p.name.contains("north")).mkString