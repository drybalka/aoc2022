package day12

import scala.io.Source
import field.*

type Board = Field[Int]

@main def task1 = {
  val input = Source
    .fromResource("day12_main")
    // .fromResource("day12_test")
    .getLines
    .map(_.toVector.map(_.toInt - 97))
    .toVector

  val S = input.posOf(-14)
  val E = input.posOf(-28)

  val heights = input.updated(S, 0).updated(E, 26)
  val steps = heights.map(_.map(_ => input.size * input(0).size))

  def dp(steps: Board, stack: List[(Pos, Int)]): Board =
    stack match
      case Nil => steps
      case head :: next =>
        val (currentPos, currentStep) = head
        if (steps(currentPos) > currentStep) then
          val newSteps = steps.updated(currentPos, currentStep)
          val newStack =
            currentPos.neighbours
              .filter(steps.inBorders)
              .filter(pos => heights(pos) - heights(currentPos) <= 1)
              .map(pos => (pos, currentStep + 1)) ::: next
          dp(newSteps, newStack)
        else dp(steps, next)

  println(dp(steps, (S, 0) :: Nil)(E))
}

@main def task2 = {
  val input = Source
    .fromResource("day12_main")
    // .fromResource("day12_test")
    .getLines
    .map(_.toVector.map(_.toInt - 97))
    .toVector

  val S = input.posOf(-14)
  val E = input.posOf(-28)

  val heights = input.updated(S, 0).updated(E, 26)
  val steps = heights.map(_.map(_ => input.size * input(0).size))

  def dp(steps: Board, stack: List[(Pos, Int)]): Board =
    stack match
      case Nil => steps
      case head :: next =>
        val (currentPos, currentStep) = head
        if (steps(currentPos) > currentStep) then
          val newSteps = steps.updated(currentPos, currentStep)
          val newStack =
            currentPos.neighbours
              .filter(steps.inBorders)
              .filter(pos => heights(pos) - heights(currentPos) >= -1)
              .map(pos => (pos, currentStep + 1)) ::: next
          dp(newSteps, newStack)
        else dp(steps, next)

  val res = dp(steps, (E, 0) :: Nil)
  val trails = for
    y <- 0 until heights.size
    x <- 0 until heights(y).size
    if heights(y)(x) == 0
  yield res(y)(x)
  println(trails.min)
}
