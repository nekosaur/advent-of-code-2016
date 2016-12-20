import io._
val input = Source.fromFile("day10.txt").getLines.toList

case class Action(typ: String, target: String, id: Int)
case class StateChange(bot: Int, actions: List[Action])
case class State(bots: Map[Int, Vector[Int]], outputs: Map[Int, Int])

val _bots = input.filter(_.startsWith("value")).map(c => {
  val pattern = "value (\\d+) goes to bot (\\d+)".r
  val m = pattern.findFirstMatchIn(c).get

  (m.group(2).toInt, m.group(1).toInt)
}).groupBy(_._1)
  .mapValues(_.map(_._2).toVector)

val _actions = input.filter(_.startsWith("bot")).map(b => {
  val pattern = "bot (\\d+) gives low to (bot|output) (\\d+) and high to (bot|output) (\\d+)".r
  val m = pattern.findFirstMatchIn(b).get
  val id :: low_target :: low :: high_target :: high :: Nil = m.subgroups

  (id.toInt, StateChange(id.toInt, List(Action("low", low_target, low.toInt), Action("high", high_target, high.toInt))))
}).toMap

def apply(state: State, change: StateChange) = {
  val chips = state.bots(change.bot).sorted

  if (chips(0) == 17 && chips(1) == 61)
    println("Bot " + change.bot)

  change.actions.foldLeft(state)((s, a) => {
    val v = if (a.typ == "low") chips(0) else chips(1)
    if (a.target == "bot") {
      val from = s.bots(change.bot).filter(_ != v)
      val to = s.bots(a.id) :+ v

      s.copy(bots = s.bots.updated(change.bot, from).updated(a.id, to))
    } else {
      val from = s.bots(change.bot).filter(_ != v)
      State(s.bots.updated(change.bot, from), s.outputs.updated(a.id, v))
    }
  })
}

def solve(bots: Map[Int, Vector[Int]], actions: Map[Int, StateChange]) = {
  def rec(state: State): State = state.bots match {
    case _ if !state.bots.exists(_._2.length > 1) => state
    case _ =>
      val bot = state.bots.find(_._2.length == 2).get
      val change = actions(bot._1)

      rec(apply(state, change))
  }

  rec(State(bots.withDefaultValue(Vector()), Map().withDefaultValue(0)))
}

val res = solve(_bots, _actions)

res.outputs(0) * res.outputs(1) * res.outputs(2)