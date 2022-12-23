package day16

import scala.io.Source

@main def task1 = {
  val day = this.getClass().getPackageName()
  val input = Source
    // .fromResource(day + "_main")
    .fromResource(day + "_test")
    .getLines
    .map {
      _ match
        case s"Valve $name has flow rate=$rate; tunnel$_ lead$_ to valve$_ $connections" =>
          (name, rate.toInt, connections.split(", ").toList)
    }
    .toList

  val rates = input.map(_.init).filter(_._2 > 0).toMap
  val connections = input.map(triple => (triple._1, triple._3)).toMap
  val valves = rates.keys.toList

  val distances = ("AA" :: valves)
    .map {
      distancesFrom(_, connections)
        .filterKeys(_ != _)
        .filterKeys((_, v) => valves.contains(v))
        .mapValues(_ + 1)
        .toMap
    }
    .reduce(_ ++ _)

  println(traverse("AA", distances, rates))
}

@main def task2 = {
  val day = this.getClass().getPackageName()
  val input = Source
    .fromResource(day + "_main")
    // .fromResource(day + "_test")
    .getLines
    .map {
      _ match
        case s"Valve $name has flow rate=$rate; tunnel$_ lead$_ to valve$_ $connections" =>
          (name, rate.toInt, connections.split(", ").toList)
    }
    .toList

  val rates = input.map(_.init).filter(_._2 > 0).toMap
  val connections = input.map(triple => (triple._1, triple._3)).toMap
  val valves = rates.keys.toList

  val distances = ("AA" :: valves)
    .map {
      distancesFrom(_, connections)
        .filterKeys(_ != _)
        .filterKeys((_, v) => valves.contains(v))
        .mapValues(_ + 1)
        .toMap
    }
    .reduce(_ ++ _)

  println(traversePair(("AA", "AA"), distances, rates))
}

def traversePair(
    start: (String, String),
    distances: Map[(String, String), Int],
    rates: Map[String, Int]
): Int =
  def filterGains(list: Set[((Int, Int), Int)]): Set[((Int, Int), Int)] =
    import scala.math.Ordering.Implicits._
    list.filterNot { case (day, gain) =>
      list.exists { case (otherDay, otherGain) =>
        otherGain > gain && otherDay >= day
      }
    }

  def step(
      front: Map[((String, String), Set[String]), Set[((Int, Int), Int)]],
      res: Int
  ): Int =
    // front.map(println)
    // println(res)
    if (front.isEmpty) then res
    else
      val newFront =
        front
          .map { case ((nodes, visited), gains) =>
            val unvisisted = rates.keys.toSet -- visited -- nodes.toList
            unvisisted.flatMap { neighbor =>
              val key1 = ((neighbor, nodes._2), visited + nodes._1)
              val key2 = ((nodes._1, neighbor), visited + nodes._2)
              val value1 = gains
                .map { case (days, gain) =>
                  val newDays =
                    (days._1 - distances((nodes._1, neighbor)), days._2)
                  val newGain = gain + rates(neighbor) * newDays._1
                  (newDays, newGain)
                }
                .filter(_._1._1 >= 0)
                .toSet
              val value2 = gains
                .map { case (days, gain) =>
                  val newDays =
                    (days._1, days._2 - distances((nodes._2, neighbor)))
                  val newGain = gain + rates(neighbor) * newDays._2
                  (newDays, newGain)
                }
                .filter(_._1._2 >= 0)
                .toSet
              List((key1 -> value1), (key2 -> value2))
            }.toMap
          }
          .reduce { case (a, b) =>
            a ++ b.map { case (k, v) => k -> (v ++ a.getOrElse(k, Nil)) }
          }
          .mapValues(filterGains)
          .toMap
      val newRes =
        newFront
          .map { case (k, v) => v.map(_._2).maxOption.getOrElse(0) }
          .maxOption
          .getOrElse(0)
      step(newFront, newRes max res)

  step(Map((start, Set.empty[String]) -> Set(((26, 26), 0))), 0)

def traverse(
    start: String,
    distances: Map[(String, String), Int],
    rates: Map[String, Int]
): Int =
  def filterGains(list: List[(Int, Int)]): List[(Int, Int)] =
    list.sortBy(_._1) match
      case (day1, gain1) :: (day2, gain2) :: tail =>
        if (day1 == day2) then filterGains((day1, gain1 max gain2) :: tail)
        else if (gain1 <= gain2) then filterGains((day2, gain2) :: tail)
        else (day1, gain1) :: filterGains((day2, gain2) :: tail)
      case other => other

  def step(front: Map[(String, Set[String]), List[(Int, Int)]], res: Int): Int =
    if (front.isEmpty) then res
    else
      val newFront =
        front
          .map { case ((node, visited), gains) =>
            val unvisisted = rates.keys.toSet -- visited - node
            unvisisted.map { neighbor =>
              val key = (neighbor, visited + node)
              val value = gains
                .map { case (days, gain) =>
                  val newDays = days - distances((node, neighbor))
                  val newGain = gain + rates(neighbor) * newDays
                  (newDays, newGain)
                }
                .filter(_._1 >= 0)
                .toList
              (key -> value)
            }.toMap
          }
          .reduce { case (a, b) =>
            a ++ b.map { case (k, v) => k -> (v ++ a.getOrElse(k, Nil)) }
          }
          .mapValues(filterGains)
          .toMap
      val newRes =
        newFront
          .map { case (k, v) => v.map(_._2).maxOption.getOrElse(0) }
          .maxOption
          .getOrElse(0)
      step(newFront, newRes max res)

  step(Map((start, Set.empty[String]) -> List((30, 0))), 0)

def distancesFrom(
    start: String,
    connections: Map[String, List[String]]
): Map[(String, String), Int] =
  def step(
      iteration: Int,
      front: List[String],
      distances: Map[(String, String), Int]
  ): Map[(String, String), Int] =
    val newFront = front.flatMap { node =>
      connections(node).filterNot(neighbor =>
        distances.contains((start, neighbor))
      )
    }
    if (newFront.isEmpty) then distances
    else
      val newDistances = distances ++ newFront.map { node =>
        ((start, node) -> (iteration + 1))
      }.toMap
      step(iteration + 1, newFront, newDistances)

  step(0, List(start), Map((start, start) -> 0))
