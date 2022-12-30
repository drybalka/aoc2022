package day24

import scala.io.Source
import field.*
import Direction.*
import scala.collection.mutable.PriorityQueue

val day = this.getClass().getPackageName()
val input = Source
  .fromResource(day + "_main")
  // .fromResource(day + "_test")
  .getLines
  .toSeq
  .tail
  .init
  .map(_.tail.init)

val sizeX = input.head.length
val sizeY = input.length
val mazeStart = (0, -1)
val mazeEnd = (sizeX - 1, sizeY)

val initialBlizzards = parseBlizzard(input)
val sizeBlizzard = lcm(sizeX, sizeY)
val allBlizzardLocations = Iterator
  .iterate(initialBlizzards)(step)
  .take(sizeBlizzard)
  .map(_.map(_.pos))
  .toIndexedSeq

@main def task1 = {
  val res = aStar(
    PriorityQueue(State(mazeStart, 0))(Ordering.by(heuristic(mazeEnd)).reverse),
    scala.collection.mutable.Set.empty[State],
    mazeEnd
  )
  println(res)
}

@main def task2 = {
  val res1 = aStar(
    PriorityQueue(State(mazeStart, 0))(Ordering.by(heuristic(mazeEnd)).reverse),
    scala.collection.mutable.Set.empty[State],
    mazeEnd
  )
  val res2 = aStar(
    PriorityQueue(State(mazeEnd, res1))(Ordering.by(heuristic(mazeStart)).reverse),
    scala.collection.mutable.Set.empty[State],
    mazeStart
  )
  val res3 = aStar(
    PriorityQueue(State(mazeStart, res2))(Ordering.by(heuristic(mazeEnd)).reverse),
    scala.collection.mutable.Set.empty[State],
    mazeEnd
  )
  println(res3)
}

case class State(pos: Pos, step: Int)

def gcd(a: Int, b: Int): Int = if b == 0 then a else gcd(b, a % b)
def lcm(a: Int, b: Int): Int = a * b / gcd(a, b)

def step(blizzards: Set[Blizzard]): Set[Blizzard] =
  blizzards.map { case Blizzard(pos, dir) =>
    val newPos = pos.in(dir) match
      case (x, y) if x < 0      => (x + sizeX, y)
      case (x, y) if x >= sizeX => (x - sizeX, y)
      case (x, y) if y < 0      => (x, y + sizeY)
      case (x, y) if y >= sizeY => (x, y - sizeY)
      case (x, y)               => (x, y)
    Blizzard(newPos, dir)
  }

def heuristic(end: Pos)(state: State): Int =
  val pos = state.pos
  state.step + ((pos.x - end.x).abs + (pos.y - end.y).abs)

def aStar(
    queue: PriorityQueue[State],
    visited: scala.collection.mutable.Set[State],
    end: Pos
): Int =
  val state = queue.dequeue()
  val State(pos, step) = state
  if (pos == end) then step
  else
    val newStep = step + 1
    val next = (pos :: pos.neighbours)
      .filter { case pos =>
        (0 <= pos.x && pos.x < sizeX && 0 <= pos.y && pos.y < sizeY) || pos == mazeEnd || pos == mazeStart
      }
      .filterNot(pos =>
        allBlizzardLocations(newStep % sizeBlizzard).contains(pos)
      )
      .map(State(_, newStep))
      .filterNot(visited.contains)
    queue ++= next
    visited ++= next
    aStar(queue, visited, end)

def parseBlizzard(input: Seq[String]): Set[Blizzard] =
  input.zipWithIndex.flatMap { (line, y) =>
    line.zipWithIndex.collect {
      case ('>', x) => Blizzard((x, y), Right)
      case ('<', x) => Blizzard((x, y), Left)
      case ('^', x) => Blizzard((x, y), Up)
      case ('v', x) => Blizzard((x, y), Down)
    }
  }.toSet

case class Blizzard(pos: Pos, dir: Direction)

enum Direction:
  case Left, Up, Right, Down

extension (dir: Direction)
  def sign =
    dir match
      case Left  => "<"
      case Up    => "^"
      case Right => ">"
      case Down  => "v"

extension (pos: Pos)
  def in(direction: Direction): Pos =
    import Direction.*
    val (x, y) = pos
    direction match
      case Left  => (x - 1, y)
      case Up    => (x, y - 1)
      case Right => (x + 1, y)
      case Down  => (x, y + 1)
