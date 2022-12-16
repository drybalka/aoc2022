import scala.io.Source

type Heights = List[List[Int]]
type Distances = List[List[Int]]
type Visibility = List[List[Boolean]]

@main def day8_1 = {
  val heights = Source
    .fromResource("day8_main")
    .getLines
    .map(_.map(_.toString.toInt).toList)
    .toList

  val res = (
    doFromLeft(visibilityFromLeft)(heights),
    doFromRight(visibilityFromLeft)(heights),
    doFromTop(visibilityFromLeft)(heights),
    doFromBottom(visibilityFromLeft)(heights)
  ).toList.reduce(combineWith(_ || _)).map(_.count(identity)).sum

  println(res)
}

@main def day8_2 = {
  val heights = Source
    .fromResource("day8_main")
    .getLines
    .map(_.map(_.toString.toInt).toList)
    .toList

  val res = (
    doFromLeft(distanceFromLeft)(heights),
    doFromRight(distanceFromLeft)(heights),
    doFromTop(distanceFromLeft)(heights),
    doFromBottom(distanceFromLeft)(heights)
  ).toList.reduce(combineWith(_ * _)).map(_.max).max

  println(res)
}

def visibilityFromLeft(
    heights: Heights
): Visibility = {
  heights.map(
    _.foldLeft(-1, List.empty[Boolean])((tuple, height) =>
      tuple match
        case (leftMax, res) => (leftMax max height, res :+ (leftMax < height))
    )._2
  )
}

def distanceFromLeft(heights: Heights): Distances = {
  heights.map(
    _.zipWithIndex
      .foldLeft(Map.empty[Int, Int], List.empty[Int])((mapRes, heightIndex) =>
        heightIndex match
          case (height, index) =>
            mapRes match
              case (prevTreePositions, res) =>
                (
                  prevTreePositions
                    .filterKeys(_ > height)
                    .toMap + (height -> index),
                  res :+ (index - prevTreePositions.getOrElse(
                    prevTreePositions.keys
                      .filter(_ >= height)
                      .minOption
                      .getOrElse(0),
                    0
                  ))
                )
      )
      ._2
  )
}

def doFromLeft[T](op: Heights => List[List[T]])(
    heights: Heights
): List[List[T]] =
  op(heights)

def doFromRight[T](op: Heights => List[List[T]])(
    heights: Heights
): List[List[T]] =
  op(heights.map(_.reverse)).map(_.reverse)

def doFromTop[T](op: Heights => List[List[T]])(
    heights: Heights
): List[List[T]] =
  op(heights.transpose).transpose

def doFromBottom[T](op: Heights => List[List[T]])(
    heights: Heights
): List[List[T]] =
  op(heights.transpose.map(_.reverse)).map(_.reverse).transpose

def combineWith[T](
    op: (T, T) => T
)(v1: List[List[T]], v2: List[List[T]]): List[List[T]] = {
  v1.zip(v2).map { case (l1, l2) => l1.zip(l2).map(op.tupled) }
}
