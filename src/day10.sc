import io._
import scala.collection.mutable
val input = Source.fromFile("day10.txt").getLines.toList

case class Bot(id: Int, low_target: String, low: Int, high_target: String, high: Int)
case class State(bot: Int, chips: List[Int])

var queue = new mutable.PriorityQueue[State]()(Ordering.by(_.chips.length))
var outputs = mutable.Map[Int, Int]()

input.filter(_.startsWith("value")).map(c => {
  val pattern = "value (\\d+) goes to bot (\\d+)".r
  val m = pattern.findFirstMatchIn(c).get

  State(m.group(2).toInt, List(m.group(1).toInt))
}).groupBy(_.bot)
  .map(t => State(t._1, t._2.flatMap(_.chips)))
  .foreach(queue.enqueue(_))

val bots = input.filter(_.startsWith("bot")).map(b => {
  val pattern = "bot (\\d+) gives low to (bot|output) (\\d+) and high to (bot|output) (\\d+)".r
  val m = pattern.findFirstMatchIn(b).get
  val id :: low_target :: low :: high_target :: high :: Nil = m.subgroups

  if (!queue.exists(_.bot == id.toInt))
    queue.enqueue(State(id.toInt, List()))

  (id.toInt, Bot(id.toInt, low_target, low.toInt, high_target, high.toInt))
}).toMap

do {
  val a = queue.dequeue()
  val b = bots(a.bot)
  val low :: high :: Nil = a.chips.sorted

  if (a.chips.contains(61) && a.chips.contains(17))
    println("Bot is " + a.bot)

  if (b.low_target == "bot") {
    val low_chips = queue.find(p => p.bot == b.low).get.chips
    queue = queue.filter(_.bot != b.low)
    queue.enqueue(State(b.low, low_chips :+ low))
  } else { outputs(b.low) = low }
  if (b.high_target == "bot") {
    val high_chips = queue.find(p => p.bot == b.high).get.chips
    queue = queue.filter(_.bot != b.high)
    queue.enqueue(State(b.high, high_chips :+ high))
  } else { outputs(b.high) = high }

} while (queue.exists(p => p.chips.length > 1))

outputs(0) * outputs(1) * outputs(2)