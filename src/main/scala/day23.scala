package day23

import scala.io.Source
import field.*
import CardinalDirection.*

val day = this.getClass().getPackageName()
val input = Source
  .fromResource(day + "_main")
  // .fromResource(day + "_test")
  .getLines
  .zipWithIndex
  .flatMap(parsePos)
  .toSet

@main def task1 = {
  // input.print()
  // println()
  val (lastElves, _) = (1 to 10).foldLeft((input, North :: South :: West :: East :: Nil)) {
    case ((elves, directionOrder), _) =>
      val newElves = plannedPos(elves, directionOrder)
      val newOrder = directionOrder.tail :+ directionOrder.head
      (newElves, newOrder)
  }
  // lastElves.print()
  println(lastElves.score)
}

@main def task2 = {
  def roundsUntilStop(elves: Set[Pos], directionOrder: Seq[CardinalDirection], round: Int): Int =
      val newElves = plannedPos(elves, directionOrder)
      val newOrder = directionOrder.tail :+ directionOrder.head
      if (elves == newElves) then round
      else roundsUntilStop(newElves, newOrder, round + 1)

  val res = roundsUntilStop(input, North :: South :: West :: East :: Nil, 1)
  println(res)
}

def parsePos(pair: (String, Int)): Seq[Pos] =
  val (line, y) = pair
  line.zipWithIndex.collect { case ('#', x) => (x, y) }

def plannedPos(
    elves: Set[Pos],
    directionOrder: Seq[CardinalDirection]
): Set[Pos] =
  def plan(pos: Pos): Pos =
    import Direction.*
    import CardinalDirection.*
    if (
      !elves(pos.in(LeftUp)) &&
      !elves(pos.in(Up)) &&
      !elves(pos.in(RightUp)) &&
      !elves(pos.in(Right)) &&
      !elves(pos.in(RightDown)) &&
      !elves(pos.in(Down)) &&
      !elves(pos.in(LeftDown)) &&
      !elves(pos.in(Left))
    )
    then pos
    else
      directionOrder
        .flatMap {
          case North =>
            if (
              !elves(pos.in(LeftUp)) &&
              !elves(pos.in(Up)) &&
              !elves(pos.in(RightUp))
            )
            then Some(pos.in(Up))
            else None
          case South =>
            if (
              !elves(pos.in(LeftDown)) &&
              !elves(pos.in(Down)) &&
              !elves(pos.in(RightDown))
            )
            then Some(pos.in(Down))
            else None
          case East =>
            if (
              !elves(pos.in(RightUp)) &&
              !elves(pos.in(Right)) &&
              !elves(pos.in(RightDown))
            )
            then Some(pos.in(Right))
            else None
          case West =>
            if (
              !elves(pos.in(LeftUp)) &&
              !elves(pos.in(Left)) &&
              !elves(pos.in(LeftDown))
            )
            then Some(pos.in(Left))
            else None
        }
        .appended(pos)
        .head

  elves.groupBy(plan).toSet.flatMap { case (to -> from) =>
    if (from.size == 1) then Set(to) else from
  }

enum CardinalDirection:
  case North, South, West, East

enum Direction:
  case Nowhere, Left, LeftUp, Up, RightUp, Right, RightDown, Down, LeftDown

extension (pos: Pos)
  def in(direction: Direction): Pos =
    import Direction.*
    val (x, y) = pos
    direction match
      case Nowhere   => (x, y)
      case Left      => (x - 1, y)
      case LeftUp    => (x - 1, y - 1)
      case Up        => (x, y - 1)
      case RightUp   => (x + 1, y - 1)
      case Right     => (x + 1, y)
      case RightDown => (x + 1, y + 1)
      case Down      => (x, y + 1)
      case LeftDown  => (x - 1, y + 1)


extension (board: Set[Pos])
  def print(): Unit =
    val minY = board.map(_._2).min
    val maxY = board.map(_._2).max
    val minX = board.map(_._1).min
    val maxX = board.map(_._1).max
    (minY to maxY).map { y =>
      val line = (minX to maxX).map { x =>
        if (board.contains((x, y))) then "#" else "."
      }.mkString
      println(line)
    }

  def score: Int =
    val minY = board.map(_._2).min
    val maxY = board.map(_._2).max
    val minX = board.map(_._1).min
    val maxX = board.map(_._1).max
    (maxX - minX + 1) * (maxY - minY + 1) - board.size
