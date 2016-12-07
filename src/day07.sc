import io._
val input = Source.fromFile("day07.txt").getLines.toList

case class IP(supernet: List[String], hypernet: List[String])

val ips = input.map(l => {
  var supernets = List[String]()
  var hypernets = List[String]()
  var tmp = ""
  for (i <- l.indices) {
    if (l(i) == '[') {
      supernets = supernets :+ tmp
      tmp = ""
    } else if (l(i) == ']') {
      hypernets = hypernets :+ tmp
      tmp = ""
    } else
      tmp = tmp + l(i)
  }

  supernets = supernets :+ tmp

  IP(supernets, hypernets)
})

ips.foldLeft(0)((c, ip) => {
  val supernet = ip.supernet.flatMap(l => l.sliding(4)).filter(s => s(0) != s(1) && s.equals(s.reverse))
  val hypernet = ip.hypernet.flatMap(l => l.sliding(4)).filter(s => s(0) != s(1) && s.equals(s.reverse))

  if (hypernet.isEmpty && supernet.nonEmpty) c + 1 else c
})

ips.foldLeft(0)((c, ip) => {
  val supernet = ip.supernet.flatMap(l => l.sliding(3)).filter(s => s(0) == s(2))
  val hypernet = ip.hypernet.flatMap(l => l.sliding(3)).filter(s => s(0) == s(2))

  if (supernet.exists(aba => hypernet.contains(aba(1).toString + aba(0) + aba(1)))) c + 1 else c
})