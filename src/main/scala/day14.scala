package day14

import scala.io.Source
import field.*

type Board = Field[Int]

@main def task1 = {
  val res = Source
    // .fromResource("day13_main")
    .fromResource("day13_test")
    .getLines


  println(res)
}
