import scala.io.Source

type Pos = (Int, Int)
extension (pos: Pos)
  def x: Int = pos._1
  def y: Int = pos._2
  def neighbours: List[Pos] =
    List((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1))
  // def left: Pos = (x - 1, y)
  // def right: Pos = (x + 1, y)
  // def top: Pos = (x, y + 1)
  // def bottom: Pos = (x, y - 1)

type Field = Vector[Vector[Int]]
extension (field: Field)
  def print(): Unit =
    field.map {
      println(_)
    }

  def updated(pos: Pos, value: Int): Field =
    field.updated(pos.y, field(pos.y).updated(pos.x, value))

  def posOf(element: Int): Pos =
    val y = field.indexWhere(_.contains(element))
    val x = field(y).indexOf(element)
    (x, y)

  def inBorders(pos: Pos): Boolean =
    field.isDefinedAt(pos.y) && field(pos.y).isDefinedAt(pos.x)

  def apply(pos: Pos): Int =
    field(pos.y)(pos.x)

@main def day12_1 = {
  val input = Source
    .fromResource("day12_main")
    // .fromResource("day12_test")
    .getLines
    .map(_.toVector.map(_.toInt - 97))
    .toVector

  val S = input.posOf(-14)
  val E = input.posOf(-28)

  val heights = input.updated(S, 0).updated(E, 26)
  val steps = heights.map(_.map(_ => input.size * input(0).size))

  def dp(steps: Field, stack: List[(Pos, Int)]): Field =
    stack match
      case Nil => steps
      case head :: next =>
        val (currentPos, currentStep) = head
        if (steps(currentPos) > currentStep) then
          val newSteps = steps.updated(currentPos, currentStep)
          val newStack =
            currentPos.neighbours
              .filter(steps.inBorders)
              .filter(pos => heights(pos) - heights(currentPos) <= 1)
              .map(pos => (pos, currentStep + 1)) ::: next
          dp(newSteps, newStack)
        else dp(steps, next)

  println(dp(steps, (S, 0) :: Nil)(E))
}

@main def day12_2 = {
  val input = Source
    .fromResource("day12_main")
    // .fromResource("day12_test")
    .getLines
    .map(_.toVector.map(_.toInt - 97))
    .toVector

  val S = input.posOf(-14)
  val E = input.posOf(-28)

  val heights = input.updated(S, 0).updated(E, 26)
  val steps = heights.map(_.map(_ => input.size * input(0).size))

  def dp(steps: Field, stack: List[(Pos, Int)]): Field =
    stack match
      case Nil => steps
      case head :: next =>
        val (currentPos, currentStep) = head
        if (steps(currentPos) > currentStep) then
          val newSteps = steps.updated(currentPos, currentStep)
          val newStack =
            currentPos.neighbours
              .filter(steps.inBorders)
              .filter(pos => heights(pos) - heights(currentPos) >= -1)
              .map(pos => (pos, currentStep + 1)) ::: next
          dp(newSteps, newStack)
        else dp(steps, next)

  val res = dp(steps, (E, 0) :: Nil)
  val trails = for
    y <- 0 until heights.size
    x <- 0 until heights(y).size
    if heights(y)(x) == 0
  yield res(y)(x)
  println(trails.min)
}
