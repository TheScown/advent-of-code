package space.scown.adventofcode
package advent2015.problems

import lib.{Files, Problem, Timer}

case class Day14(input: Vector[String]) extends Problem {

  override def solve1(): Unit = {
    val reindeer = parse()
    val targetTime = 2503

    val result = reindeer.map {
      r => distanceAtTime(targetTime, r)
    }.max

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val reindeer = parse()
    val targetTime = 2503

    val initialMap = Map.from(reindeer.map(_.name -> 0))
    val scores = (1 to targetTime).foldLeft(initialMap) { (map, currentTime) =>
      val distances = reindeer.map(r => (r, distanceAtTime(currentTime, r)))
      val maxDistance = distances.map(_._2).max
      val winners = distances.filter(_._2 == maxDistance).map(_._1)
      map ++ winners.map(winner => winner.name -> (map(winner.name) + 1))
    }

    val winningScore = scores.values.max

    println(s"Result 2: $winningScore")
  }

  private def distanceAtTime(targetTime: Int, r: Reindeer) = {
    val wholePeriods = targetTime / r.combinedPeriod
    val remainder = targetTime % r.combinedPeriod

    val additionalDistance = if (remainder >= r.duration) r.distancePerPeriod else remainder * r.speed

    wholePeriods * r.distancePerPeriod + additionalDistance
  }

  def parse(): Vector[Reindeer] = {
    val pattern = "([A-Z][a-z]+)\\D+(\\d+)\\D+(\\d+)\\D+(\\d+).*$".r

    input.map {
      case pattern(name, speed, duration, restPeriod) => Reindeer(name, speed.toInt, duration.toInt, restPeriod.toInt)
    }
  }

  case class Reindeer(name: String, speed: Int, duration: Int, restPeriod: Int) {
    val combinedPeriod: Int = duration + restPeriod
    val distancePerPeriod: Int = speed * duration
  }

}

case object Day14 extends App {
  val input = Files.lines("2015/day14.txt")
  val problem = Day14(input)
  Timer.time(() => problem.solve1())
  Timer.time(() => problem.solve2())
}
