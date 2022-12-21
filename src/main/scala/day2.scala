package day2

import scala.io.Source

@main def task1 = {
  val res = Source
    .fromResource("day2_main")
    .getLines
    .map(_ match {
      case "A X" => 1 + 3
      case "A Y" => 2 + 6
      case "A Z" => 3 + 0
      case "B X" => 1 + 0
      case "B Y" => 2 + 3
      case "B Z" => 3 + 6
      case "C X" => 1 + 6
      case "C Y" => 2 + 0
      case "C Z" => 3 + 3
    })
    .sum
  println(res)
}

@main def task2 = {
  val res = Source
    .fromResource("day2_main")
    .getLines
    .map(_ match {
      case "A X" => 3 + 0
      case "A Y" => 1 + 3
      case "A Z" => 2 + 6
      case "B X" => 1 + 0
      case "B Y" => 2 + 3
      case "B Z" => 3 + 6
      case "C X" => 2 + 0
      case "C Y" => 3 + 3
      case "C Z" => 1 + 6
    })
    .sum
  println(res)
}
