import scala.io.Source

case class Pos(val x: Int, val y: Int):
  def in(direction: Direction): Pos =
    direction match
      case Nowhere      => Pos(x, y)
      case Left      => Pos(x - 1, y)
      case LeftUp    => Pos(x - 1, y + 1)
      case Up        => Pos(x, y + 1)
      case RightUp   => Pos(x + 1, y + 1)
      case Right     => Pos(x + 1, y)
      case RightDown => Pos(x + 1, y - 1)
      case Down      => Pos(x, y - 1)
      case LeftDown  => Pos(x - 1, y - 1)

  def from(direction: Direction): Pos =
    direction match
      case Nowhere      => Pos(x, y)
      case Left      => Pos(x + 1, y)
      case LeftUp    => Pos(x + 1, y - 1)
      case Up        => Pos(x, y - 1)
      case RightUp   => Pos(x - 1, y - 1)
      case Right     => Pos(x - 1, y)
      case RightDown => Pos(x - 1, y + 1)
      case Down      => Pos(x, y + 1)
      case LeftDown  => Pos(x + 1, y + 1)

sealed trait Direction
case object Nowhere extends Direction
case object Left extends Direction
case object LeftUp extends Direction
case object Up extends Direction
case object RightUp extends Direction
case object Right extends Direction
case object RightDown extends Direction
case object Down extends Direction
case object LeftDown extends Direction

extension (tail: Direction)
  def newTail(headMovement: Direction): Direction =
    (tail, headMovement) match
      case (Nowhere, direction) => direction
      case (direction, Nowhere) => direction

      case (Left, Right) | (LeftUp, RightDown) | (Up, Down) |
          (RightUp, LeftDown) | (Right, Left) | (RightDown, LeftUp) |
          (Down, Up) | (LeftDown, RightUp) =>
        Nowhere

      case (Left, Up) | (Up, Left) | (LeftUp, LeftUp)             => LeftUp
      case (Right, Up) | (Up, Right) | (RightUp, RightUp)         => RightUp
      case (Left, Down) | (Down, Left) | (LeftDown, LeftDown)     => LeftDown
      case (Right, Down) | (Down, Right) | (RightDown, RightDown) => RightDown

      case (Left, Left) | (Left, LeftUp) | (Left, LeftDown) | (LeftUp, Left) |
          (LeftDown, Left) | (LeftUp, LeftDown) | (LeftDown, LeftUp) |
          (LeftUp, Down) | (Down, LeftUp) | (LeftDown, Up) | (Up, LeftDown) =>
        Left
      case (Right, Right) | (Right, RightUp) | (Right, RightDown) |
          (RightUp, Right) | (RightDown, Right) | (RightUp, RightDown) |
          (RightDown, RightUp) | (RightUp, Down) | (Down, RightUp) |
          (RightDown, Up) | (Up, RightDown) =>
        Right

      case (Up, Up) | (Up, LeftUp) | (LeftUp, Up) | (Up, RightUp) |
          (RightUp, Up) | (RightUp, LeftUp) | (LeftUp, RightUp) |
          (Left, RightUp) | (RightUp, Left) | (Right, LeftUp) |
          (LeftUp, Right) =>
        Up
      case (Down, Down) | (Down, LeftDown) | (LeftDown, Down) |
          (Down, RightDown) | (RightDown, Down) | (RightDown, LeftDown) |
          (LeftDown, RightDown) | (Left, RightDown) | (RightDown, Left) |
          (Right, LeftDown) | (LeftDown, Right) =>
        Down

  def tailMovement(headMovement: Direction): Direction =
    (tail, headMovement) match
      case (Left, Left) | (LeftUp, LeftDown) | (LeftDown, LeftUp)       => Left
      case (Right, Right) | (RightUp, RightDown) | (RightDown, RightUp) => Right
      case (Up, Up) | (LeftUp, RightUp) | (RightUp, LeftUp)             => Up
      case (Down, Down) | (LeftDown, RightDown) | (RightDown, LeftDown) => Down

      case (Left, LeftUp) | (LeftUp, Left) | (LeftUp, LeftUp) | (LeftUp, Up) |
          (Up, LeftUp) =>
        LeftUp
      case (Left, LeftDown) | (LeftDown, Left) | (LeftDown, LeftDown) |
          (LeftDown, Down) | (Down, LeftDown) =>
        LeftDown
      case (Right, RightUp) | (RightUp, Right) | (RightUp, RightUp) |
          (RightUp, Up) | (Up, RightUp) =>
        RightUp
      case (Right, RightDown) | (RightDown, Right) | (RightDown, RightDown) |
          (RightDown, Down) | (Down, RightDown) =>
        RightDown

      case (_, _) => Nowhere

case class Rope(
    val head: Pos,
    val fromTail: Direction
):
  def tail: Pos = head.from(fromTail)

  def move(direction: Direction): Rope =
    Rope(head in direction, fromTail.newTail(direction))

object Rope:
  val init = Rope(Pos(0, 0), Nowhere)

case class LongRope(chains: List[Rope]):
  def tail: Pos = chains.last.tail

  def move(direction: Direction): LongRope =
    val newChains = chains
      .foldLeft(direction, List.empty[Rope])((tuple, chain) =>
        val (direction, res) = tuple
        val newChain = chain.move(direction)
        val newDirection = chain.fromTail.tailMovement(direction)
        (newDirection, newChain :: res)
      )
      ._2
      .reverse
    LongRope(newChains)

  def print(): Unit =
    val size = 11
    val emptyField = List.fill(2 * size)("." * 2 * size)
    def markField(
        field: List[String],
        mark: Char,
        x: Int,
        y: Int
    ): List[String] =
      field.updated(y, field(y).updated(x, mark))

    chains.zipWithIndex.reverse
      .foldLeft(
        markField(
          emptyField,
          'H',
          size + chains.head.head.x,
          size - chains.head.head.y
        )
      )((field, tuple) =>
        val (chain, index) = tuple
        markField(
          field,
          (index + 1).toString.head,
          size + chain.tail.x,
          size - chain.tail.y
        )
      )
      .map(println)

object LongRope:
  val init = LongRope(List.fill(9)(Rope(Pos(0, 0), Nowhere)))

@main def day9_1 = {
  val res = Source
    .fromResource("day9_main")
    // .fromResource("day9_test")
    .getLines
    .foldLeft(Rope.init, Set.empty[Pos])((ropeVisited, line) =>
      val (rope, visited) = ropeVisited
      val (direction, steps) =
        line.split(" ") match
          case Array(dir, steps) =>
            val direction = dir match
              case "U" => Up
              case "D" => Down
              case "L" => Left
              case "R" => Right
            (direction, steps.toInt)

      def doMove(
          rope: Rope,
          visited: Set[Pos],
          direction: Direction,
          steps: Int
      ): (Rope, Set[Pos]) =
        steps match
          case 0 => (rope, visited)
          case _ =>
            val newRope = rope.move(direction)
            doMove(newRope, visited + newRope.tail, direction, steps - 1)

      doMove(rope, visited, direction, steps)
    )
    ._2
    .size

  println(res)
}

@main def day9_2 = {
  val res = Source
    .fromResource("day9_main")
    // .fromResource("day9_test")
    .getLines
    .foldLeft(LongRope.init, Set.empty[Pos])((ropeVisited, line) =>
      val (rope, visited) = ropeVisited
      val (direction, steps) =
        line.split(" ") match
          case Array(dir, steps) =>
            val direction = dir match
              case "U" => Up
              case "D" => Down
              case "L" => Left
              case "R" => Right
            (direction, steps.toInt)

      def doMove(
          rope: LongRope,
          visited: Set[Pos],
          direction: Direction,
          steps: Int
      ): (LongRope, Set[Pos]) =
        steps match
          case 0 => (rope, visited)
          case _ =>
            val newRope = rope.move(direction)
            doMove(newRope, visited + newRope.tail, direction, steps - 1)

      doMove(rope, visited, direction, steps)
    )
    ._2.size

  // res.print()

  println(res)
}
