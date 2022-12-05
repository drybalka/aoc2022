import scala.io.Source

@main def day5_1 = {
  val (input, operations) = Source
    .fromResource("day5_main")
    .getLines
    .span(_.length != 0)

  val state = input
    .map(_.drop(1).sliding(1, 4).toList)
    .toList
    .transpose
    .map(_.dropWhile(_ == " ").dropRight(1))

  operations
    .drop(1)
    .foldLeft(state)((state, line) => {
      val op = line.split(" ")
      val quantity = op(1).toInt
      val from = op(3).toInt - 1
      val to = op(5).toInt - 1
      state
        .updated(to, state(from).take(quantity).reverse ::: state(to))
        .updated(from, state(from).drop(quantity))
    })
    .map(_(0))
    .foreach(print)
}

@main def day5_2 = {
  val (input, operations) = Source
    .fromResource("day5_main")
    .getLines
    .span(_.length != 0)

  val state = input
    .map(_.drop(1).sliding(1, 4).toList)
    .toList
    .transpose
    .map(_.dropWhile(_ == " ").dropRight(1))

  operations
    .drop(1)
    .foldLeft(state)((state, line) => {
      val op = line.split(" ")
      val quantity = op(1).toInt
      val from = op(3).toInt - 1
      val to = op(5).toInt - 1
      state
        .updated(to, state(from).take(quantity) ::: state(to))
        .updated(from, state(from).drop(quantity))
    })
    .map(_(0))
    .foreach(print)
}
