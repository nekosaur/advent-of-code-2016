val _input = "10111011111001111"

def generate(input: String, length: Int): String = input match {
  case in if in.length >= length => input.take(length)
  case in => generate(in + "0" + in.reverseMap { case '1' => '0'; case _ => '1' }, length)
}

def checksum(input: String) = {
  def rec(xs: List[Char]): List[Char] = xs match {
    case ys if ys.length % 2 != 0 => ys
    case ys => rec(ys.grouped(2).toList.map { case p if p(0) == p(1) => '1'; case _ => '0' })
  }

  rec(input.toList)
}

def solve(input: String, length: Int) = checksum(generate(input, length)).mkString

solve(_input, 272)
solve(_input, 35651584)