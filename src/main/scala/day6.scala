package day6

import scala.io.Source

@main def task1 = {
  val res = Source
    .fromResource("day6_main")
    .bufferedReader
    .readLine
    .sliding(4, 1)
    .indexWhere(_.toSet.size == 4)

    println(res + 4)
}

@main def task2 = {
  val res = Source
    .fromResource("day6_main")
    .bufferedReader
    .readLine
    .sliding(14, 1)
    .indexWhere(_.toSet.size == 14)

    println(res + 14)
}
