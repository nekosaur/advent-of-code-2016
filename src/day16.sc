import java.util.concurrent.TimeUnit

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

def div(i: Int, n: Int): Int = if (i % 2 != 0) n else div(i / 2, n + 1)

def parallel(data: List[Char], length: Int) = {
  val chunk_size = math.pow(2, div(length, 0)).toInt

  data.grouped(chunk_size)
      .toList
      .par
      .flatMap(checksum)
}

def faster(input: List[Char], n: Int) = {
  val chunk_size = math.pow(2, div(n, 0)).toInt
  def rec(xs: List[Char], o: List[Char]): List[Char] = xs match {
    case Nil => o
    case ys =>
      val chunk = ys.take(chunk_size)
      val bit = if (chunk.count(_ == '1') % 2 == 0) '1' else '0'
      rec(ys.drop(chunk_size), o :+ bit)
  }

  rec(input, List())
}

def solve(input: String, length: Int) = parallel(generate(input, length), length).mkString
def solve_faster(input: String, length: Int) = faster(generate(input, length), length).mkString

//solve(_input, 272)
//solve(_input, 35651584)
solve_faster(_input, 272)
solve_faster(_input, 35651584)
