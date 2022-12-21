package day15

import scala.io.Source

@main def task1 = {
  val Y = 2000000
  val res = Source
    .fromResource("day15_main")
    // .fromResource("day15_test")
    .getLines
    .map { line =>
      val split = line.split("(x=|, y=)|:").toList
      (split(1).toInt, split(2).toInt, split(4).toInt, split(5).toInt)
    }
    .flatMap { (sx, sy, bx, by) =>
      val radius = Math.abs(sx - bx) + Math.abs(sy - by)
      val rp = radius - Math.abs(Y - sy)
      if (rp > 0) then Some((sx - rp, sx + rp)) else None
    }
    .toList
    .sortBy(_._1)
    .foldLeft(List.empty[(Int, Int)]) { (res, el) =>
      res match
        case Nil => el :: Nil
        case head :: next =>
          if (head._2 >= el._1) then (head._1, head._2 max el._2) :: next
          else el :: head :: next
    }
    .map((l, r) => r - l)
    .sum

  println(res)
}

@main def task2 = {
  val border = 4000000
  val diamonds = Source
    .fromResource("day15_main")
    // .fromResource("day15_test")
    .getLines
    .map { line =>
      val split = line.split("(x=|, y=)|:").toList
      (split(1).toInt, split(2).toInt, split(4).toInt, split(5).toInt)
    }
    .map { (sx, sy, bx, by) =>
      val radius = Math.abs(sx - bx) + Math.abs(sy - by)
      (sx, sy, radius + 1)
    }
    .toList

  val candidates = diamonds
    .combinations(2)
    .flatMap(pair => intersections(pair(0), pair(1)))
    .filter((x,y) => 0 < x && x < border && 0 < y && y < border)
    .toList

  val res = candidates.find { (x, y) =>
    diamonds.forall { (sx, sy, r) =>
      Math.abs(x - sx) + Math.abs(y - sy) >= r
    }
  }.get

  println(res)
  println(res._1 * 4000000L + res._2)
}

def intersections(s1: (Int, Int, Int), s2: (Int, Int, Int)): List[(Int, Int)] =
  val (s1x, s1y, r1) = s1
  val (s2x, s2y, r2) = s2

  def gen(a: Int, b: Int, c: Int, d: Int): Option[(Int, Int)] =
    if (a * b - c * d == 0) then None
    else
      val x = s1y - s2y + b * r1 - d * r2 + a * b * s1x - c * d * s2x
      val y = s1x - s2x + a * r1 - c * r2 + a * b * s1y - c * d * s2y
      if (x % 2 != 0 || y % 2 != 0) then None
      else
        val div = a * b - c * d
        Some((x / div, y / div))

  (List.fill(4)(1) ::: List.fill(4)(-1))
    .combinations(4)
    .flatMap(_.permutations)
    .flatMap(el => gen(el(0), el(1), el(2), el(3)))
    .toList
