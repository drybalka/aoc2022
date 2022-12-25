package day18

import scala.io.Source

val day = this.getClass().getPackageName()
val input = Source
  .fromResource(day + "_main")
  // .fromResource(day + "_test")
  .getLines
  .map { case s"$x,$y,$z" => (x.toInt, y.toInt, z.toInt) }
  .toSet

type Droplet = Set[(Int, Int, Int)]

@main def task1 = {
  println(input.surfaceArea)
}

@main def task2 = {
  val extSize = input.map(_.toList.max).max + 1

  println(
    exteriorCube(extSize, input).surfaceArea - 6 * (extSize + 2) * (extSize + 2)
  )

}

def exteriorCube(size: Int, drop: Droplet): Droplet =
  def step(stack: List[(Int, Int, Int)], acc: Droplet): Droplet =
    stack match
      case Nil => acc
      case head :: next =>
        if (acc.contains(head)) then step(next, acc)
        else
          step(
            adjacentCubes(head).filter { case (x, y, z) =>
              -1 <= x && x <= size &&
              -1 <= y && y <= size &&
              -1 <= z && z <= size &&
              !drop.contains((x, y, z))
            }.toList ::: stack,
            acc + head
          )

  step(List((-1, -1, -1)), Set.empty[(Int, Int, Int)])

extension (drop: Droplet)
  def surfaceArea: Int =
    drop.foldLeft(0) { case (acc, cube) =>
      acc + adjacentCubes(cube).count(!drop.contains(_))
    }

def adjacentCubes(cube: (Int, Int, Int)): Set[(Int, Int, Int)] =
  val (x, y, z) = cube
  Set(
    (x + 1, y, z),
    (x - 1, y, z),
    (x, y + 1, z),
    (x, y - 1, z),
    (x, y, z + 1),
    (x, y, z - 1)
  )
