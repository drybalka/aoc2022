package day21

import scala.io.Source

val day = this.getClass().getPackageName()
val input = Source
  .fromResource(day + "_main")
  // .fromResource(day + "_test")
  .getLines
  .toSeq

@main def task1 = {
  val monkeys = input.map(parseOp).toMap
  val res = monkeys("root")(monkeys)
  println(res)
}

@main def task2 = {
  val cleanInput = removeRoot(
    input
      .filterNot("humn: [0-9]*".r.matches)
  )
  val monkeys = flipTree(cleanInput, "humn").map(parseOp).toMap

  val res = monkeys("humn")(monkeys)
  println(res)
}

type Operation = Map[String, Any] => Long

def parseOp(line: String): (String, Operation) =
  line match
    case s"$name: $left $op $right" =>
      (name, genOp(left, op, right))
    case s"$name: $num" => (name, _ => num.toLong)

def genOp(left: String, op: String, right: String): Operation =
  (map: Map[String, Any]) =>
    val l = map(left).asInstanceOf[Operation](map)
    val r = map(right).asInstanceOf[Operation](map)
    op match
      case "+" => l + r
      case "-" => l - r
      case "*" => l * r
      case "/" => l / r

def removeRoot(monkeys: Seq[String]): Seq[String] =
  val (root, other) = monkeys.partition(_.startsWith("root:"))
  val (l, r) = root.head match
    case s"root: $left $op $right" => (left, right)
  other.map(_.replaceAllLiterally(l, r))

def invertOp(line: String, name: String): String =
  line match
    case s"$left: $center $op $right" if center == name =>
      op match
        case "+" => s"$name: $left - $right"
        case "-" => s"$name: $left + $right"
        case "*" => s"$name: $left / $right"
        case "/" => s"$name: $left * $right"
    case s"$left: $center $op $right" if right == name =>
      op match
        case "+" => s"$name: $left - $center"
        case "-" => s"$name: $center - $left"
        case "*" => s"$name: $left / $center"
        case "/" => s"$name: $center / $left"

def flipTree(monkeys: Seq[String], name: String): Seq[String] =
  val (line, rest) = monkeys.partition(s".*:.*$name.*".r.matches)
  if (line.isEmpty) then monkeys
  else invertOp(line.head, name) +: flipTree(rest, line.head.split(":")(0))
