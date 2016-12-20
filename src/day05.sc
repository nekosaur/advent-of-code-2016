import java.security._

def md5(s: String): String = {
  val hash = MessageDigest.getInstance("MD5")
  val hashed = hash.digest(s.getBytes())

  hashed.map {
    case b if (0xFF & b) < 0x10 => "0" + Integer.toHexString(0xFF & b)
    case b => Integer.toHexString(0xFF & b)
  }.mkString
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
