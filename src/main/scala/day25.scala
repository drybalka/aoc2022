package day25

import scala.io.Source

val day = this.getClass().getPackageName()
val input = Source
  .fromResource(day + "_main")
  // .fromResource(day + "_test")
  .getLines
  .toSeq

@main def task1 = {
  val res = input
    // .tapEach(println)
    .map(decodeSnafu)
    // .tapEach(println)
    .sum

  println(encodeSnafu(res))
}

def decodeSnafu(num: String): Long =
  def decodeSnafuChar(char: Char): Long = char match
    case '2' => 2
    case '1' => 1
    case '0' => 0
    case '-' => -1
    case '=' => -2

  num.reverse
    .foldLeft((0L, 1L)) { case ((acc, power), char) =>
      val newAcc = acc + decodeSnafuChar(char) * power
      val newPower = power * 5
      (newAcc, newPower)
    }
    ._1

def encodeSnafu(num: Long): String =
  def encodeSnafuChar(digit: Long): Char = digit match
    case 0 => '0'
    case 1 => '1'
    case 2 => '2'
    case 3 => '='
    case 4 => '-'

  def step(num: Long, acc: String): String =
    if (num == 0) then acc
    else
      val mod = num % 5
      val div = num / 5
      val carryOver = if (mod > 2) then 1 else 0
      step(div + carryOver, acc + encodeSnafuChar(mod))

  step(num, "").reverse
