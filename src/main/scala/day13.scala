import scala.io.Source

case class NestedList(items: List[NestedList | Int]):
  def prepend(item: NestedList | Int): NestedList =
    NestedList(item :: this.items)

  override def toString(): String =
    "[" + items.map(_.toString).reduceOption(_ + "," + _).getOrElse("") + "]"

  def inOrder(other: NestedList): Boolean =
    def compare(
        left: NestedList | Int,
        right: NestedList | Int
    ): Option[Boolean] =
      (left, right) match
        case (NestedList(Nil), NestedList(Nil))                  => None
        case (NestedList(leftHead :: leftTail), NestedList(Nil)) => Some(false)
        case (NestedList(Nil), NestedList(rightHead :: rightTail)) => Some(true)
        case (
              NestedList(leftHead :: leftTail),
              NestedList(rightHead :: rightTail)
            ) =>
          compare(leftHead, rightHead) match
            case Some(value) => Some(value)
            case None => compare(NestedList(leftTail), NestedList(rightTail))
        case (l: Int, NestedList(r)) =>
          compare(NestedList(l :: Nil), NestedList(r))
        case (NestedList(l), r: Int) =>
          compare(NestedList(l), NestedList(r :: Nil))
        case (l: Int, r: Int) =>
          if (l < r) then Some(true)
          else if (l > r) then Some(false)
          else None

    compare(this, other).get

object NestedList:
  def empty = NestedList(Nil)
  def fromString(str: String): NestedList =
    def fromTokens(
        tokens: List[String],
        stack: List[NestedList]
    ): NestedList =
      tokens match
        case "]" :: next => fromTokens(next, NestedList.empty :: stack)
        case "[" :: Nil  => stack.head
        case "[" :: next =>
          val newStack = stack match
            case current :: parent :: tail =>
              parent.prepend(current) :: tail
            case _ => ???
          fromTokens(next, newStack)
        case number :: next if number.forall(_.isDigit) =>
          val newStack = stack.head.prepend(number.toInt) :: stack.tail
          fromTokens(next, newStack)
        case _ => ???

    val split = str.split("(,|(?<=\\[)|(?=\\]))").toList.reverse
    fromTokens(split, Nil)

@main def day13_1 = {
  val res = Source
    .fromResource("day13_main")
    // .fromResource("day13_test")
    .getLines
    .grouped(3)
    .map(_.init)
    .map { pair =>
      val first = NestedList.fromString(pair(0))
      val second = NestedList.fromString(pair(1))
      first.inOrder(second)
    }
    .zipWithIndex
    .filter(_._1)
    .map(_._2 + 1)
    .sum

  println(res)
}

@main def day13_2 = {
  val res = Source
    .fromResource("day13_main")
    // .fromResource("day13_test")
    .getLines
    .filterNot(_ == "")
    .concat(List("[[2]]", "[[6]]"))
    .map(NestedList.fromString(_))
    .toList
    .sortWith(_ inOrder _)

  val a = res.indexOf(NestedList.fromString("[[2]]")) + 1
  val b = res.indexOf(NestedList.fromString("[[6]]")) + 1
  println(a * b)
}
