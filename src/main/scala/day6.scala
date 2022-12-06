import scala.io.Source

@main def day6_1 = {
  val res = Source
    .fromResource("day6_main")
    .bufferedReader
    .readLine
    .sliding(4, 1)
    .indexWhere(_.toSet.size == 4)

    println(res + 4)
}

@main def day6_2 = {
  val res = Source
    .fromResource("day6_main")
    .bufferedReader
    .readLine
    .sliding(14, 1)
    .indexWhere(_.toSet.size == 14)

    println(res + 14)
}
