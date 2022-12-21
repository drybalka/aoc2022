package day4

import scala.io.Source

@main def task1 = {
  val res = Source
    .fromResource("day4_main")
    .getLines
    .map(line => {
      val split = line.split(',')
      val left = split(0).split('-')
      val right = split(1).split('-')
      val l1 = left(0).toInt
      val r1 = left(1).toInt
      val l2 = right(0).toInt
      val r2 = right(1).toInt
      if ((l1 <= l2 && r2 <= r1) || (l2 <= l1 && r1 <= r2)) {
        1
      } else {
        0
      }
    })
    .sum
  println(res)
}

@main def task2 = {
  val res = Source
    .fromResource("day4_main")
    .getLines
    .map(line => {
      val split = line.split(',')
      val left = split(0).split('-')
      val right = split(1).split('-')
      val l1 = left(0).toInt
      val r1 = left(1).toInt
      val l2 = right(0).toInt
      val r2 = right(1).toInt
      if ((l1 <= l2 && l2 <= r1) || (l2 <= l1 && l1 <= r2)) {
        1
      } else {
        0
      }
    })
    .sum
  println(res)
}
