package day22

import scala.io.Source
import field.*

val day = this.getClass().getPackageName()
val input = Source
  .fromResource(day + "_main")
  // .fromResource(day + "_test")
  .getLines
  .toSeq

@main def task1 = {
  val map = genMap(input.take(input.size - 2))
  val path = genPath(input.last)
  val startPos = (map(1).indexOf('.'), 1)
  val startDirection = Direction.Right

  val (lastPos, lastDirection) = path.foldLeft((startPos, startDirection)) {
    case ((pos, direction), command) =>
      command match
        case Command.Move =>
          val newPos = pos.inDirection(direction).jump(direction, map)
          map(newPos) match
            case '.' => (newPos, direction)
            case '#' => (pos, direction)
        case Command.ClockWise => (pos, rotateClockWise(direction))
        case Command.CounterClockWise =>
          (pos, rotateCounterClockWise(direction))
  }

  val res = 1000 * lastPos.y + 4 * lastPos.x + lastDirection.score
  println(res)
}

@main def task2 = {
  val map = genMap(input.take(input.size - 2))
  val path = genPath(input.last)
  val startPos = (map(1).indexOf('.'), 1)
  val startDirection = Direction.Right

  val (lastPos, lastDirection) = path.foldLeft((startPos, startDirection)) {
    case ((pos, direction), command) =>
      command match
        case Command.Move =>
          val (newPos, newDirection) =
            pos.inDirection(direction).jumpCube(direction, map)
          map(newPos) match
            case '.' => (newPos, newDirection)
            case '#' => (pos, direction)
        case Command.ClockWise => (pos, rotateClockWise(direction))
        case Command.CounterClockWise =>
          (pos, rotateCounterClockWise(direction))
  }

  val res = 1000 * lastPos.y + 4 * lastPos.x + lastDirection.score
  println(res)
}

type Board = Field[Char]

def genMap(input: Seq[String]): Board =
  val width = input.map(_.length).max
  val height = input.length
  val mapWithBorders =
    (" " * width +: input :+ " " * width)
      .map(_.padTo(width, ' '))
      .map(" " + _ + " ")
      .map(_.toVector)
      .toVector
  mapWithBorders

enum Command:
  case Move, ClockWise, CounterClockWise

enum Direction:
  case Left, Right, Up, Down

extension (direction: Direction)
  def score: Int = direction match
    case Direction.Right => 0
    case Direction.Down  => 1
    case Direction.Left  => 2
    case Direction.Up    => 3

extension (pos: Pos)
  def inDirection(direction: Direction): Pos =
    val (x, y) = pos
    direction match
      case Direction.Right => (x + 1, y)
      case Direction.Left  => (x - 1, y)
      case Direction.Up    => (x, y - 1)
      case Direction.Down  => (x, y + 1)

  def jump(direction: Direction, map: Board): Pos =
    if (map(pos) != ' ') then pos
    else
      direction match
        case Direction.Down => (pos.x, pos.vertical(map).indexWhere(_ != ' '))
        case Direction.Up => (pos.x, pos.vertical(map).lastIndexWhere(_ != ' '))
        case Direction.Right =>
          (pos.horizontal(map).indexWhere(_ != ' '), pos.y)
        case Direction.Left =>
          (pos.horizontal(map).lastIndexWhere(_ != ' '), pos.y)

  def jumpCube(direction: Direction, map: Board): (Pos, Direction) =
    if (map(pos) != ' ') then (pos, direction)
    else
      (pos, direction) match
        case ((x, 0), Direction.Up) if (50 < x && x <= 100) =>
          ((1, 100 + x), Direction.Right)
        case ((0, y), Direction.Left) if (150 < y && y <= 200) =>
          ((y - 100, 1), Direction.Down)

        case ((x, 0), Direction.Up) if (100 < x && x <= 150) =>
          ((x - 100, 200), Direction.Up)
        case ((x, 201), Direction.Down) if (0 < x && x <= 50) =>
          ((x + 100, 1), Direction.Down)

        case ((50, y), Direction.Left) if (0 < y && y <= 50) =>
          ((1, 151 - y), Direction.Right)
        case ((0, y), Direction.Left) if (100 < y && y <= 150) =>
          ((51, 151 - y), Direction.Right)

        case ((151, y), Direction.Right) if (0 < y && y <= 50) =>
          ((100, 151 - y), Direction.Left)
        case ((101, y), Direction.Right) if (100 < y && y <= 150) =>
          ((150, 151 - y), Direction.Left)

        case ((x, 51), Direction.Down) if (100 < x && x <= 150) =>
          ((100, x - 50), Direction.Left)
        case ((101, y), Direction.Right) if (50 < y && y <= 100) =>
          ((y + 50, 50), Direction.Up)

        case ((50, y), Direction.Left) if (50 < y && y <= 100) =>
          ((y - 50, 101), Direction.Down)
        case ((x, 100), Direction.Up) if (0 < x && x <= 50) =>
          ((51, x + 50), Direction.Right)

        case ((x, 151), Direction.Down) if (50 < x && x <= 100) =>
          ((50, x + 100), Direction.Left)
        case ((51, y), Direction.Right) if (150 < y && y <= 200) =>
          ((y - 100, 150), Direction.Up)

  def side: Pos = ((pos.x - 1) / 50, (pos.y - 1) / 50)

  def vertical(map: Board): Vector[Char] = map.map(_(pos.x))
  def horizontal(map: Board): Vector[Char] = map(pos.y)

def rotateClockWise(direction: Direction): Direction =
  direction match
    case Direction.Left  => Direction.Up
    case Direction.Up    => Direction.Right
    case Direction.Right => Direction.Down
    case Direction.Down  => Direction.Left

def rotateCounterClockWise(direction: Direction): Direction =
  direction match
    case Direction.Left  => Direction.Down
    case Direction.Up    => Direction.Left
    case Direction.Right => Direction.Up
    case Direction.Down  => Direction.Right

def genPath(input: String): Seq[Command] =
  input.split("(?<=L)|(?=L)|(?<=R)|(?=R)").flatMap {
    _ match
      case "R" => Seq(Command.ClockWise)
      case "L" => Seq(Command.CounterClockWise)
      case num => Seq.fill(num.toInt)(Command.Move)
  }
