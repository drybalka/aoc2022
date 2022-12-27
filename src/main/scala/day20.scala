package day20

import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.IndexedBuffer

val day = this.getClass().getPackageName()
val input = Source
  .fromResource(day + "_main")
  // .fromResource(day + "_test")
  .getLines
  .map(_.toLong)
  .toSeq

@main def task1 = {
  val in = input.zipWithIndex
  val buf = ArrayBuffer.from(in)
  for pair <- in do
    val n = pair._1
    val oldI = buf.indexOf(pair)
    val newI = math.floorMod(oldI + n, input.size - 1).toInt
    buf.remove(oldI)
    buf.insert(newI, pair)

  val i0 = buf.indexWhere(_._1 == 0)
  println(
    buf((1000 + i0) % input.size)._1 + buf((2000 + i0) % input.size)._1 + buf(
      (3000 + i0) % input.size
    )._1
  )
}

@main def task2 = {
  val in = input.map(_ * 811589153).zipWithIndex
  val buf = ArrayBuffer.from(in)
  for _ <- 0 until 10 do
    for pair <- in do
      val n = pair._1
      val oldI = buf.indexOf(pair)
      val newI = math.floorMod(oldI + n, input.size - 1).toInt
      buf.remove(oldI)
      buf.insert(newI, pair)

  val i0 = buf.indexWhere(_._1 == 0)
  println(
    buf((1000 + i0) % input.size)._1 + buf((2000 + i0) % input.size)._1 + buf(
      (3000 + i0) % input.size
    )._1
  )
}
