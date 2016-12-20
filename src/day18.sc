var _input = ".^^^^^.^^.^^^.^...^..^^.^.^..^^^^^^^^^^..^...^^.^..^^^^..^^^^...^.^.^^^^^^^^....^..^^^^^^.^^^.^^^.^^"

def char(row: Vector[Char], i: Int) = {
  row.slice(i, i + 3) match {
    case Seq(x, _, y) if x == y => '.'
    case _ => '^'
  }
}

def gen(input: Vector[Char]) = {
  val row = '.' +: input :+ '.'
  input.indices.map(char(row, _)).toVector
}

def solve(input: String, n: Int) = Stream.from(0)
                                         .scanLeft(_input.toVector)((r, _) => gen(r))
                                         .take(n)
                                         .flatten
                                         .count(_ == '.')

solve(_input, 40)
solve(_input, 400000)
