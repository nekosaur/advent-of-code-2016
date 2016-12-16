val _input = "10111011111001111"

def generate(input: String, length: Int) = {
  def rec(xs: List[Char]): List[Char] = xs match {
    case ys if ys.length >= length => ys.take(length)
    case ys => rec(ys ++ List('0') ++ ys.reverseMap { case '1' => '0'; case _ => '1' })
  }

  rec(input.toList)
}

def checksum(xs: List[Char]): List[Char] = xs match {
  case ys if ys.length % 2 != 0 => ys
  case ys => checksum(ys.grouped(2).toList.map { case p if p(0) == p(1) => '1'; case _ => '0' })
}

def solve(input: String, length: Int) = checksum(generate(input, length)).mkString

solve(_input, 272)
solve(_input, 35651584)
