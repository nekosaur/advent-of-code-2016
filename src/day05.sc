import java.security._

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

val pwd = Array[Char]('.','.','.','.','.','.','.','.')
var index = 0
val input = "ffykfhsq"
var found = 0
while (found < 8) {
  val hash = md5(input + index.toString)

  if (hash.startsWith("00000")) {
    val index = hash.drop(5).head.asDigit
    if (index >= 0 && index <= 7 && pwd(index) == '.') {
      val char = hash.drop(6).head
      pwd(index) = char
      found = found + 1
    }
  }

  index = index + 1
}

pwd.mkString
