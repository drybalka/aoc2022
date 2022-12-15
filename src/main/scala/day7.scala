import scala.io.Source

@main def day7_1 = {
  val (dirSizes, _) = Source
    .fromResource("day7_main")
    .getLines
    .foldLeft(Map(List("/") -> 0), Nil: List[String])((tuple, line) => {
      tuple match {
        case (dirSizes, path) =>
          line match {
            case s"$$ ls"      => (dirSizes, path)
            case s"$$ cd .."   => (dirSizes, path.tail)
            case s"$$ cd $dir" => (dirSizes, dir :: path)
            case s"dir $dir"   => (dirSizes + ((dir :: path) -> 0), path)
            case s"$size $name" =>
              (updatedSizes(dirSizes, path, size.toInt), path)
          }
      }
    })

  val res = dirSizes.values.filter(_ < 100_000).sum

  println(res)
}

@main def day7_2 = {
  val (dirSizes, _) = Source
    .fromResource("day7_main")
    .getLines
    .foldLeft(Map(List("/") -> 0), Nil: List[String])((tuple, line) => {
      tuple match {
        case (dirSizes, path) =>
          line match {
            case s"$$ ls"      => (dirSizes, path)
            case s"$$ cd .."   => (dirSizes, path.tail)
            case s"$$ cd $dir" => (dirSizes, dir :: path)
            case s"dir $dir"   => (dirSizes + ((dir :: path) -> 0), path)
            case s"$size $name" =>
              (updatedSizes(dirSizes, path, size.toInt), path)
          }
      }
    })

  val requiredSpace = dirSizes(List("/")) - 40_000_000

  val res = dirSizes.values.filter(_ > requiredSpace).min

  println(res)
}

def updatedSizes(
    dirSizes: Map[List[String], Int],
    path: List[String],
    size: Int
): Map[List[String], Int] = {
  path match
    case Nil => dirSizes
    case dir :: tail =>
      updatedSizes(dirSizes + (path -> (dirSizes(path) + size)), tail, size)
}
