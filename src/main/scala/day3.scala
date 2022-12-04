import scala.io.Source

@main def day3_1 = {
  val res = Source
    .fromResource("day3_main")
    .getLines
    .map(line => {
      val (left, right) = line.splitAt(line.length / 2)
      left.toSet intersect right.toSet
    })
    .map(set => {
      val char = set.head
      if (char.isUpper) {
        (char & 31) + 26
      } else {
        char & 31
      }
    })
    .sum
  println(res)
}

@main def day3_2 = {
  val res = Source
    .fromResource("day3_main")
    .getLines
    .grouped(3)
    .map(triple => {
      triple.map(_.toSet).reduce(_ intersect _)
    })
    .map(set => {
      val char = set.head
      if (char.isUpper) {
        (char & 31) + 26
      } else {
        char & 31
      }
    })
    .sum
  println(res)
}
