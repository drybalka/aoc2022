package day19

import scala.io.Source
import scala.collection.mutable.PriorityQueue

val day = this.getClass().getPackageName()
val input = Source
  .fromResource(day + "_main")
  // .fromResource(day + "_test")
  .getLines
  .map {
    case s"Blueprint $i: Each ore robot costs $ore_ore ore. Each clay robot costs $clay_ore ore. Each obsidian robot costs $obsidian_ore ore and $obsidian_clay clay. Each geode robot costs $geode_ore ore and $geode_obsidian obsidian." =>
      (
        i.toInt,
        ore_ore.toInt,
        clay_ore.toInt,
        obsidian_ore.toInt,
        obsidian_clay.toInt,
        geode_ore.toInt,
        geode_obsidian.toInt
      )
  }
  .toList

@main def task1 = {
  given days: Int = 24
  input.map(procesBlueprint.tupled).map(println)
  println(input.map(bp => bp._1 * procesBlueprint.tupled(bp)).sum)
}

@main def task2 = {
  given days: Int = 32
  println(input.take(3).map(procesBlueprint.tupled).product)
}

def procesBlueprint(using days: Int)(
    i: Int,
    ore_ore: Int,
    clay_ore: Int,
    obsidian_ore: Int,
    obsidian_clay: Int,
    geode_ore: Int,
    geode_obsidian: Int
): Int =

  case class State(
      day: Int,
      ore: Int,
      clay: Int,
      obs: Int,
      geo: Int,
      oreB: Int,
      clayB: Int,
      obsB: Int,
      geoB: Int
  ):
    def decDay: State = copy(day = day - 1)
    def produce: State =
      copy(
        ore = ore + oreB,
        clay = clay + clayB,
        obs = obs + obsB,
        geo = geo + geoB
      )

    def makeOreB: Option[State] =
      if (ore >= ore_ore) then
        Some(copy(ore = ore - ore_ore - 1, oreB = oreB + 1))
      else None

    def makeClayB: Option[State] =
      if (ore >= clay_ore) then
        Some(copy(ore = ore - clay_ore, clay = clay - 1, clayB = clayB + 1))
      else None

    def makeObsB: Option[State] =
      if (ore >= obsidian_ore && clay >= obsidian_clay) then
        Some(
          copy(
            ore = ore - obsidian_ore,
            clay = clay - obsidian_clay,
            obs = obs - 1,
            obsB = obsB + 1
          )
        )
      else None

    def makeGeoB: Option[State] =
      if (ore >= geode_ore && obs >= geode_obsidian) then
        Some(
          copy(
            ore = ore - geode_ore,
            obs = obs - geode_obsidian,
            geo = geo - 1,
            geoB = geoB + 1
          )
        )
      else None

  given Ordering[State] = Ordering.by(heuristic)

  def heuristic(state: State): Int =
    extension (state: State)
      def prodOreB: State =
        state.copy(ore = state.ore - 1, oreB = state.oreB + 1)

      def prodClayB: State =
        if (state.ore >= clay_ore) then
          state.copy(
            ore = state.ore - clay_ore,
            clay = state.clay - 1,
            clayB = state.clayB + 1
          )
        else state

      def prodObsB: State =
        if (state.clay >= obsidian_clay) then
          state.copy(
            clay = state.clay - obsidian_clay,
            obs = state.obs - 1,
            obsB = state.obsB + 1
          )
        else state

      def prodGeoB: State =
        if (state.obs >= geode_obsidian) then
          state.copy(
            obs = state.obs - geode_obsidian,
            geo = state.geo - 1,
            geoB = state.geoB + 1
          )
        else state

    if (state.day == 0) then state.geo
    else heuristic(state.prodGeoB.prodObsB.prodClayB.prodOreB.produce.decDay)

  def aStar(queue: PriorityQueue[State]): Int =
    // println(queue)
    val state = queue.dequeue()
    if (state.day == 0) then state.geo
    else
      queue += state.produce.decDay
      if (state.makeOreB.isDefined) then
        queue += state.makeOreB.get.produce.decDay
      if (state.makeClayB.isDefined) then
        queue += state.makeClayB.get.produce.decDay
      if (state.makeObsB.isDefined) then
        queue += state.makeObsB.get.produce.decDay
      if (state.makeGeoB.isDefined) then
        queue += state.makeGeoB.get.produce.decDay
      aStar(queue)

  aStar(
    PriorityQueue(
      State(
        day = days,
        ore = 0,
        clay = 0,
        obs = 0,
        geo = 0,
        oreB = 1,
        clayB = 0,
        obsB = 0,
        geoB = 0
      )
    )
  )

  // def betterThan(one: State, other: State): Boolean =
  //   import scala.math.Ordering.Implicits._
  //   (
  //     one._1 >= other._1 &&
  //     one._2 >= other._2 &&
  //     one._3 >= other._3 &&
  //     one._4 >= other._4 &&
  //     one._5 >= other._5 &&
  //     one._6 >= other._6 &&
  //     one._7 >= other._7 &&
  //     one._8 >= other._8
  //   ) &&
  //   (
  //     one._1 > other._1 ||
  //     one._2 > other._2 ||
  //     one._3 > other._3 ||
  //     one._4 > other._4 ||
  //     one._5 > other._5 ||
  //     one._6 > other._6 ||
  //     one._7 > other._7 ||
  //     one._8 > other._8
  //   )
  //
  // def step(
  //     time: Int,
  //     states: Set[State]
  // ): Int =
  //   // println((time, states))
  //   // println()
  //   if time == 0 then states.map(_._4).max
  //   else
  //     val newStates = states.flatMap{ state =>
  //       produceOre(state) ::
  //       produceOreB(state).flatMap(produceOre) ::
  //       produceClayB(state).flatMap(produceOre) ::
  //       produceObsB(state).flatMap(produceOre) ::
  //       produceGeoB(state).flatMap(produceOre) ::
  //       Nil
  //     }.flatten
  //
  //     val filtered = newStates.filter { one =>
  //       !newStates.exists(other => betterThan(other, one))
  //     }
  //
  //     step(time - 1, filtered)
  //
  //
  // step(2, Set((0, 0, 0, 0, 1, 0, 0, 0)))
