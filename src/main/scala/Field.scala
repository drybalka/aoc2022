package field

type Pos = (Int, Int)
extension (pos: Pos)
  def x: Int = pos._1
  def y: Int = pos._2
  def neighbours: List[Pos] =
    List((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1))
  def left: Pos = (x - 1, y)
  def right: Pos = (x + 1, y)
  def top: Pos = (x, y + 1)
  def bottom: Pos = (x, y - 1)

type Field[A] = Vector[Vector[A]]
extension[A] (field: Field[A])
  def print(): Unit =
    field.map {
      println(_)
    }

  def updated(pos: Pos, value: A): Field[A] =
    field.updated(pos.y, field(pos.y).updated(pos.x, value))

  def posOf(element: A): Pos =
    val y = field.indexWhere(_.contains(element))
    val x = field(y).indexOf(element)
    (x, y)

  def inBorders(pos: Pos): Boolean =
    field.isDefinedAt(pos.y) && field(pos.y).isDefinedAt(pos.x)

  def apply(pos: Pos): A =
    field(pos.y)(pos.x)
