package space.scown.adventofcode
package advent2021

import lib.{Files, Problem}

case class Day7(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val crabs = input.head.split(",").map(_.toInt).toVector
    val minPosition = crabs.min
    val maxPosition = crabs.max

    val costs = (minPosition to maxPosition).map { position =>
      crabs.map(c => math.abs(position - c)).sum
    }

    val result = costs.min

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val crabs = input.head.split(",").map(_.toInt).toVector
    val minPosition = crabs.min
    val maxPosition = crabs.max

    val costs = (minPosition to maxPosition).map { position =>
      crabs.map(c => {
        val distance = math.abs(position - c)
        distance * (distance + 1) / 2
      }).sum
    }

    val result = costs.min

    println(s"Result 2: $result")
  }
}

case object Day7 extends App {
  val input = Files.lines("2021/day7.txt")
  val problem = Day7(input)
  problem.solve1()
  problem.solve2()
}
