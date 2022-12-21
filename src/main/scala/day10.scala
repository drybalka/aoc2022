package day10

import scala.io.Source

type State = (Int, Int, List[Int])
type Screen = (Int, Int, String)

@main def task1 = {
  val res = Source
    .fromResource("day10_main")
    // .fromResource("day10_test")
    .getLines
    .foldLeft(1, 1, List.empty[Int])((state, line) => {
      line match
        case "noop"     => process(state, 1, 0)
        case s"addx $v" => process(state, 2, v.toInt)
    })
    ._3.takeRight(6).sum

  println(res)
}

@main def task2 = {
  val res = Source
    .fromResource("day10_main")
    // .fromResource("day10_test")
    .getLines
    .foldLeft(1, 1, "")((screen, line) => {
      line match
        case "noop"     => processScreen(screen, 1, 0)
        case s"addx $v" => processScreen(screen, 2, v.toInt)
    })
    ._3.grouped(40).toList.map(println)
}

def process(state: State, dcycle: Int, dx: Int): State =
  val (x, cycle, strengths) = state
  val newX = if (dcycle == 0) then x + dx else x

  if (dcycle == 0) then (newX, cycle, strengths)
  else
    val newStrengths =
      if (cycle % 40 == 20) then (cycle * x) :: strengths
      else strengths
    process((newX, cycle + 1, newStrengths), dcycle - 1, dx)

def processScreen(screen: Screen, dcycle: Int, dx: Int): Screen =
  val (x, cycle, res) = screen
  val newX = if (dcycle == 0) then x + dx else x

  if (dcycle == 0) then (newX, cycle, res)
  else
    val newRes =
      if (((cycle - 1) % 40 - newX).abs <= 1) then res + "#"
      else res + " "
    processScreen((newX, cycle + 1, newRes), dcycle - 1, dx)
