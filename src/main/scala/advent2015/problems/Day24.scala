package space.scown.adventofcode
package advent2015.problems

import lib.{Files, Problem, Timer}

case class Day24(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val result = solve(3)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val result = solve(4)

    println(s"Result 2: $result")
  }

  private def solve(groups: Int): Long = {
    val packages = input.map(_.toLong)
    val targetWeight = packages.sum / groups
    val largestMinGroupSize = packages.size / groups

    val result = (1 to largestMinGroupSize).flatMap(n => packages.combinations(n)
      .filter(c => c.sum == targetWeight)
    ).sorted {
      (x: Vector[Long], y: Vector[Long]) => if (x.size == y.size) x.product.compareTo(y.product)
        else x.size.compareTo(y.size)
    }.head

    result.product
  }

  case class State(
    remainingPackages: Vector[Int],
    remainingPackageIndex: Int,
    currentGroup: Vector[Int],
    completedGroups: Vector[Vector[Int]]
  )
}

case object Day24 extends App {
  val input = Files.lines("2015/day24.txt")
  val problem = Day24(input)
  Timer.time(() => problem.solve1())
  Timer.time(() => problem.solve2())
}
