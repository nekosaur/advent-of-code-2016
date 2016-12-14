import java.security.MessageDigest

def md5(s: String): String = {
  val hash = MessageDigest.getInstance("MD5");
  val hashed = hash.digest(s.getBytes())
  val hex = new StringBuilder()

  for (i <- 0 until hashed.length) {
    if ((0xFF & hashed(i)) < 0x10)
      hex.append("0")
    hex.append(Integer.toHexString(0xFF & hashed(i)))
  }

  hex.toString
}

val _input = "ngcjuoqr"

def solve(input: String, stretch: Int) = {
  var hash = Map[Int, String]()

  def md5stretch(salt: String, i: Int, n: Int) = i match {
    case key if hash.contains(key) => hash(key)
    case _ =>
      val h = (0 until n).foldLeft(salt + i.toString)((h, _) => md5(h))
      hash = hash.updated(i, h)
      h
  }

  def three(s: String) = {
    def rec(xs: List[Char], n: Int): Option[Char] = xs match {
      case Nil => Option.empty
      case y :: _ if n == 2 => Option.apply(y)
      case _ :: Nil => Option.empty
      case y :: ys if y == ys.head => rec(ys, n + 1)
      case _ :: ys => rec(ys, 0)
    }

    rec(s.toList, 0)
  }

  def five(s: String, c: Char) = s.contains(c.toString * 5)

  Stream.from(0).map(i => (i, md5stretch(input, i, stretch))).filter(t => {
    val (index, hash) = t
    val c = three(hash)

    c.isDefined && (index + 1 to index + 1000).map(j => md5stretch(input, j, stretch)).exists(five(_, c.get))
  }).drop(63).head
}

println(solve(_input, 1))
println(solve(_input, 1 + 2016))