package space.scown.adventofcode
package advent2019.problems

import lib.{Complex, Files, Grid, Problem}

case class Day10(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val grid = Grid(input.map(_.toVector))
    val asteroids = grid.zipWithIndex.filter(_._1 == '#').map(_._2).toSet

    val result = asteroids.map { position =>
      linesOfSight(position, asteroids).size
    }.max

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val grid = Grid(input.map(_.toVector))
    val asteroids = grid.zipWithIndex.filter(_._1 == '#').map(_._2).toSet

    val (monitoringStationLocation, chosenLinesOfSight) = asteroids.map { position =>
      (position, linesOfSight(position, asteroids))
    }.maxBy(_._2.size)

    val asteroidsToHit = chosenLinesOfSight.map { case (angle, asteroids) =>
      (angle, asteroids.minBy(_ mh monitoringStationLocation))
    }.toVector

    val sortedAsteroids = asteroidsToHit.sortBy(_._1).reverse
    val resultAsteroid = sortedAsteroids.apply(199)._2
    val result = (resultAsteroid.re * 100) + (-resultAsteroid.im)

    println(s"Result 2: $result")
  }

  private def linesOfSight(position: Complex[Int], asteroids: Set[Complex[Int]]): Map[Double, Set[Complex[Int]]] = {
    val otherPositions = asteroids - position

    val byAngle = otherPositions.groupBy { op =>
      val delta = op - position
      // Measure angles from north – this is useful for part 2
      math.atan2(delta.re, -delta.im)
    }

    byAngle
  }
}

case object Day10 extends App {
  val input = Files.lines("2019/day10.txt")
  val problem = Day10(input)
  problem.solve1()
  problem.solve2()
}
