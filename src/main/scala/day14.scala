package day14

import scala.io.Source
import field.*

type Board = Field[Boolean]

@main def task1 = {
  val input = Source
    .fromResource("day14_main")
    // .fromResource("day14_test")
    .getLines
    .flatMap {
      _.split(" -> ")
        .map(toPos)
        .sliding(2)
        .map { pair => (pair(0), pair(1)) }
    }
    .toList

  val minX = input.map { pair =>
    val (l, r) = pair
    l.x min r.x
  }.min
  val maxX = input.map { pair =>
    val (l, r) = pair
    l.x max r.x
  }.max
  val minY = 0
  val maxY = input.map { pair =>
    val (l, r) = pair
    l.y max r.y
  }.max

  val width = maxX - minX + 3
  val height = maxY - minY + 1
  val start = (501 - minX, 0)

  val board = input
    .map { pair =>
      val ((x1, y1), (x2, y2)) = pair
      ((x1 - minX + 1, y1), (x2 - minX + 1, y2))
    }
    .foldLeft(Vector.fill(height, width)(false)) { (board, pair) =>
      board.fillRect(pair)
    }
    
  // board.print(if _ then "#" else ".")

  val (resBoard, resCount) = sandfall(board, start :: Nil, 0)
  println(resCount)
  // resBoard.print(if _ then "#" else ".")
}

@main def task2 = {
  val input = Source
    .fromResource("day14_main")
    // .fromResource("day14_test")
    .getLines
    .flatMap {
      _.split(" -> ")
        .map(toPos)
        .sliding(2)
        .map { pair => (pair(0), pair(1)) }
    }
    .toList

  val minY = 0
  val maxY = input.map { pair =>
    val (l, r) = pair
    l.y max r.y
  }.max + 2
  val minX = input.map { pair =>
    val (l, r) = pair
    l.x min r.x
  }.min min (500 - maxY)
  val maxX = input.map { pair =>
    val (l, r) = pair
    l.x max r.x
  }.max max (500 + maxY)

  val width = maxX - minX + 3
  val height = maxY - minY + 1
  val start = (501 - minX, 0)

  val board = input
    .prepended(((minX,maxY),(maxX,maxY)))
    .map { pair =>
      val ((x1, y1), (x2, y2)) = pair
      ((x1 - minX + 1, y1), (x2 - minX + 1, y2))
    }
    .foldLeft(Vector.fill(height, width)(false)) { (board, pair) =>
      board.fillRect(pair)
    }
    
  // board.print(if _ then "#" else ".")

  val (resBoard, resCount) = sandfill(board, start :: Nil, 0)
  println(resCount)
  // resBoard.print(if _ then "#" else ".")
}

def sandfill(board: Board, path: List[Pos], count: Int): (Board, Int) =
  path match
    case Nil => (board, count)
    case (x, y) :: tail =>
      val down = (x, y + 1)
      val downleft = (x - 1, y + 1)
      val downright = (x + 1, y + 1)
      if (!board(down)) then sandfill(board, down :: path, count)
      else if (!board(downleft)) then sandfill(board, downleft :: path, count)
      else if (!board(downright)) then sandfill(board, downright :: path, count)
      else sandfill(board.updated(path.head, true), path.tail, count + 1)

def sandfall(board: Board, path: List[Pos], count: Int): (Board, Int) =
  val (x, y) = path.head
  val down = (x, y + 1)
  val downleft = (x - 1, y + 1)
  val downright = (x + 1, y + 1)
  if (!board.inBorders(down)) then (board, count)
  else if (!board(down)) then sandfall(board, down :: path, count)
  else if (!board(downleft)) then sandfall(board, downleft :: path, count)
  else if (!board(downright)) then sandfall(board, downright :: path, count)
  else sandfall(board.updated(path.head, true), path.tail, count + 1)

def toPos(str: String): Pos =
  str.split(",").toList match
    case List(x, y) => (x.toInt, y.toInt)
    case _          => ???

extension (board: Board)
  def fill(pos: Pos): Board =
    board.updated(pos, true)

  def fillRect(pair: (Pos, Pos)): Board =
    val (pos1, pos2) = pair
    val lx = pos1.x min pos2.x
    val rx = pos1.x max pos2.x
    val ly = pos1.y min pos2.y
    val ry = pos1.y max pos2.y
    val rect = for
      x <- lx to rx
      y <- ly to ry
    yield (x, y)
    rect.foldLeft(board) { (board, pos) =>
      board.fill(pos)
    }
