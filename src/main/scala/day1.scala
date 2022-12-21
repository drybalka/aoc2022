package day1

import scala.io.Source

@main def task1 = {
  val res = Source
    .fromResource("day1_main")
    .getLines
    .foldLeft(0 :: Nil)((res, line) => {
      line match
        case "" => 0 :: res
        case _  => (line.toInt + res.head) :: res.tail
    })
    .max
  println(res)
}

@main def task2 = {
  val res = Source
    .fromResource("day1_main")
    .getLines
    .foldLeft(0 :: Nil)((res, line) => {
      line match
        case "" => 0 :: res
        case _  => (line.toInt + res.head) :: res.tail
    })
  println(topN(3, res).sum)
}

def topN(n: Int, list: List[Int]): List[Int] = {
  import scala.collection.mutable.PriorityQueue
  val pq = PriorityQueue(list.take(n)*)(Ordering[Int].reverse)
  list
    .drop(n)
    .foreach(v => {
      pq += v
      pq.dequeue()
    })
  pq.toList
}
