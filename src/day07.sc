import io._
val input = Source.fromFile("day07.txt").getLines.toList

case class IP(supernets: List[String], hypernets: List[String])

val ips = input.map(l => {
  val supernets = "^(\\w+)|\\](\\w+)\\[|(\\w+)$".r
    .findAllMatchIn(l)
    .flatMap(m => m.subgroups.filter(_ != null))

  var hypernets = "\\[(\\w+)\\]".r
    .findAllMatchIn(l)
    .flatMap(m => m.subgroups)

  IP(supernets.toList, hypernets.toList)
})

ips.count(ip => {
  val supernets = ip.supernets
    .flatMap(l => l.sliding(4))
    .filter(s => s(0) != s(1) && s.equals(s.reverse))

  val hypernets = ip.hypernets
    .flatMap(l => l.sliding(4))
    .filter(s => s(0) != s(1) && s.equals(s.reverse))

  hypernets.isEmpty && supernets.nonEmpty
})

ips.count(ip => {
  val supernets = ip.supernets
    .flatMap(l => l.sliding(3))
    .filter(s => s(0) == s(2))

  val hypernets = ip.hypernets
    .flatMap(l => l.sliding(3))
    .filter(s => s(0) == s(2))

  supernets.exists(aba => hypernets.contains(aba(1).toString + aba(0) + aba(1)))
})