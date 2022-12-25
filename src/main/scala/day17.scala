package day17

import scala.io.Source
import field.*

enum PieceType:
  def constructPiece(pos: Pos): Piece =
    this match
      case Horizontal =>
        Piece(List(pos, pos.right, pos.right.right, pos.right.right.right))
      case Vertical => Piece(List(pos, pos.top, pos.top.top, pos.top.top.top))
      case Plus =>
        Piece(
          List(
            pos.right,
            pos.top,
            pos.right.top,
            pos.right.top.top,
            pos.top.right.right
          )
        )
      case Corner =>
        Piece(
          List(
            pos,
            pos.right,
            pos.right.right,
            pos.right.right.top,
            pos.right.right.top.top
          )
        )
      case Square =>
        Piece(
          List(
            pos,
            pos.right,
            pos.top,
            pos.top.right
          )
        )

  case Horizontal, Vertical, Plus, Corner, Square

object PieceType:
  val order = Vector(Horizontal, Plus, Corner, Vertical, Square)
  def apply(n: Int): PieceType = order(n % 5)

case class Piece(tiles: List[Pos]):
  def isLegal(board: Set[Pos]): Boolean =
    tiles.forall(pos =>
      pos.x >= 0 && pos.x < 7 && pos.y > 0 && !board.contains(pos)
    )

  def maxY = tiles.map(_._2).max

  def left: Piece = Piece(tiles.map(_.left))
  def right: Piece = Piece(tiles.map(_.right))
  def top: Piece = Piece(tiles.map(_.top))
  def bottom: Piece = Piece(tiles.map(_.bottom))

@main def task1 = {
  val day = this.getClass().getPackageName()
  val input = Source
    .fromResource(day + "_main")
    // .fromResource(day + "_test")
    .bufferedReader
    .readLine

  val (resBoard, lastStep) =
    dropN(2022, PieceType.Horizontal, Set.empty[Pos], input, 0)

  // resBoard.print()
  println(resBoard.map(_._2).max)
}

@main def task2 = {
  val day = this.getClass().getPackageName()
  val input = Source
    .fromResource(day + "_main")
    // .fromResource(day + "_test")
    .bufferedReader
    .readLine

  // val (resBoard, lastStep) = dropN(PieceType.order.size * 5, PieceType.Horizontal, Set.empty[Pos], input, 0)
  val pattern =
    findRepeating(Set.empty[Pos], input, PieceType.Horizontal, List((0, 0, 0, Set.empty[Pos])))
  pattern.take(5).map(println)
  pattern.takeRight(5).map(println)
  // pattern.head._4.print()

  // dropN(9 * 5, PieceType.Horizontal, Set.empty[Pos], input, 0)._1.printTop()
  // dropN(360 * 5, PieceType.Horizontal, Set.empty[Pos], input, 0)._1.printTop()
  // dropN(711 * 5, PieceType.Horizontal, Set.empty[Pos], input, 0)._1.printTop()


  def heightAtIteration(iteration: Long): Long =
    val h0 = pattern(pattern.size - 1)._3
    val i0 = pattern(pattern.size - 1)._2
    val dH = pattern(0)._3 - h0
    val dI = pattern(0)._2 - i0

    (iteration - i0) / dI * dH + pattern.reverse(((iteration - i0) % dI).toInt)._3

  println(heightAtIteration(200000000000L))
  // val iter = 900
  // println(heightAtIteration(iter))
  // val (resBoard, lastStep) =
  //   dropN(iter * 5, PieceType.Horizontal, Set.empty[Pos], input, 0)
  // println(resBoard.map(_._2).max)
}

extension (board: Set[Pos])
  def print(): Unit =
    val maxY = board.map(_._2).max
    (maxY + 2 to 1 by -1).map { y =>
      val line = (0 to 6).map { x =>
        if (board.contains((x, y))) then "#" else "."
      }.mkString
      println(line)
    }

  def printTop(): Unit =
    val maxY = board.map(_._2).max
    (maxY + 2 to maxY - 10 by -1).map { y =>
      val line = (0 to 6).map { x =>
        if (board.contains((x, y))) then "#" else "."
      }.mkString
      println(line)
    }

def findRepeating(
    board: Set[Pos],
    winds: String,
    startingPieceType: PieceType,
    heights: List[(Int, Int, Int, Set[Pos])]
): List[(Int, Int, Int, Set[Pos])] =
  val (startingStep, iteration, _, _) = heights.head
  val (resBoard, lastStep) = dropN(
    PieceType.order.size,
    PieceType.Horizontal,
    board,
    winds,
    startingStep
  )
  val newHeight = resBoard.map(_._2).maxOption.getOrElse(0)
  val wind = lastStep % winds.size
  val boardTop = resBoard.filter(newHeight - _._2 < 5).map{ case (x, y) => (x, newHeight - y)}
  val patternLength = heights.indexWhere(el => el._1 == wind && el._4 == boardTop)
  if (patternLength > -1) then (wind, iteration + 1, newHeight, boardTop) :: heights.take(patternLength + 1)
  else
    findRepeating(
      resBoard,
      winds,
      startingPieceType,
      (wind, iteration + 1, newHeight, boardTop) :: heights
    )

def dropN(
    n: Int,
    startingPieceType: PieceType,
    board: Set[Pos],
    winds: String,
    startingStep: Int
): (Set[Pos], Int) =
  val offset = PieceType.order.indexOf(startingPieceType)
  (0 until n).foldLeft((board, startingStep)) { case ((board, step), i) =>
    val height = board.map(_._2).maxOption.getOrElse(0)
    val piece = PieceType(offset + i).constructPiece((2, height + 4))
    dropPiece(board, piece, winds, step)
  }

def dropPiece(
    board: Set[Pos],
    piece: Piece,
    winds: String,
    step: Int
): (Set[Pos], Int) =
  val wind = winds(step % winds.size)
  val blownPiece = moveWind(wind, piece)
  val afterWindPiece = if (blownPiece.isLegal(board)) then blownPiece else piece
  val fallenPiece = afterWindPiece.bottom
  if (fallenPiece.isLegal(board)) then
    dropPiece(board, fallenPiece, winds, step + 1)
  else
    val newBoard = afterWindPiece.tiles.foldLeft(board) { case (board, tile) =>
      board + tile
    }
    (newBoard, step + 1)

// def height(
//     board: Set[Pos],
//     winds: String,
//     piece: Piece,
//     pieceNum: Int,
//     step: Int,
//     maxHeight: Int
// ): Int =
//   val wind = winds(step % winds.size)
//   val blownPiece = moveWind(wind, piece)
//   val afterWindPiece = if (blownPiece.isLegal(board)) then blownPiece else piece
//   val fallenPiece = afterWindPiece.bottom
//   if (fallenPiece.isLegal(board)) then
//     height(board, winds, fallenPiece, pieceNum, step + 1, maxHeight)
//   else
//     val newBoard = afterWindPiece.tiles.foldLeft(board) { case (board, tile) =>
//       board + tile
//     }
//     val newMaxHeight = maxHeight max piece.maxY
//     if (pieceNum == 2021) then
//       // newBoard.print()
//       newMaxHeight
//     else
//       val newPiece =
//         PieceType.order(pieceNum + 1).constructPiece((2, newMaxHeight + 4))
//       height(newBoard, winds, newPiece, pieceNum + 1, step + 1, newMaxHeight)

def moveWind(wind: Char, piece: Piece): Piece =
  wind match
    case '<' => piece.left
    case '>' => piece.right
    case _   => ???
