import scala.io.Source

@main def day11_1 = {
  val monkeyMap = Source
    .fromResource("day11_main")
    // .fromResource("day11_test")
    .getLines
    .grouped(7)
    .map { Monkey.fromInput }
    .map { m => (m.index -> m) }
    .toMap

  val after20 = (1 to 20).foldLeft(monkeyMap) { (monkeyMap, _) =>
    (0 until monkeyMap.size).foldLeft(monkeyMap) { (monkeyMap, key) =>
      val monkey = monkeyMap(key)
      monkey.items.foldLeft(monkeyMap) { (monkeyMap, item) =>
        val newItem = monkey.op(item) / 3
        val receiver =
          if (newItem % monkey.test == 0) then monkey.trueI else monkey.falseI
        monkeyMap + (receiver -> monkeyMap(receiver).receive(newItem))
      } + (key -> monkey.emptyItems)
    }
  }

  val res = (0 until after20.size).map(after20(_).activity)

  println(res.toList.sorted(Ordering.Int.reverse).take(2).product)
}

@main def day11_2 = {
  val monkeyMap = Source
    .fromResource("day11_main")
    // .fromResource("day11_test")
    .getLines
    .grouped(7)
    .map { Monkey.fromInput }
    .map { m => (m.index -> m) }
    .toMap

  val lcm = (0 until monkeyMap.size).foldLeft(1L)(_ * monkeyMap(_).test)

  val after10000 = (1 to 10000).foldLeft(monkeyMap) { (monkeyMap, _) =>
    (0 until monkeyMap.size).foldLeft(monkeyMap) { (monkeyMap, key) =>
      val monkey = monkeyMap(key)
      monkey.items.foldLeft(monkeyMap) { (monkeyMap, item) =>
        val newItem = monkey.op(item) % lcm
        val receiver =
          if (newItem % monkey.test == 0) then monkey.trueI else monkey.falseI
        monkeyMap + (receiver -> monkeyMap(receiver).receive(newItem))
      } + (key -> monkey.emptyItems)
    }
  }

  val res = (0 until after10000.size).map(after10000(_).activity)

  println(res.toList.sorted(Ordering.Int.reverse).take(2).map(_.toLong).product)
}

case class Monkey(
    index: Int,
    items: List[Long],
    op: Long => Long,
    test: Long,
    trueI: Int,
    falseI: Int,
    activity: Int
):
  def receive(item: Long): Monkey =
    Monkey(index, items.appended(item), op, test, trueI, falseI, activity)

  def emptyItems: Monkey =
    Monkey(
      index,
      List.empty[Long],
      op,
      test,
      trueI,
      falseI,
      activity + items.size
    )

case object Monkey:
  def fromInput(input: Seq[String]): Monkey =
    val index = input(0).trim match {
      case s"Monkey $i:" => i.toInt
    }
    val items = input(1).trim match {
      case s"Starting items: $list" =>
        list.split(", ").map(_.toLong).toList
    }
    val op = input(2).trim match {
      case s"Operation: new = $left $op $right" =>
        buildOperation(left, op, right)
    }
    val test = input(3).trim match {
      case s"Test: divisible by $div" => div.toInt
    }
    val trueI = input(4).trim match {
      case s"If true: throw to monkey $i" => i.toInt
    }
    val falseI = input(5).trim match {
      case s"If false: throw to monkey $i" => i.toInt
    }
    Monkey(index, items, op, test, trueI, falseI, 0)

  def buildOperation(left: String, op: String, right: String): Long => Long =
    x =>
      val lhs = if (left == "old") then x else left.toLong
      val rhs = if (right == "old") then x else right.toLong
      op match
        case "*" => lhs * rhs
        case "+" => lhs + rhs
