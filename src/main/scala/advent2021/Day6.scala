package space.scown.adventofcode
package advent2021

import lib.{Files, Problem}

case class Day6(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val initialCounts = parse()

    val finalCounts = (0 until 80).foldLeft(initialCounts)(step)

    val result = finalCounts.values.sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val initialCounts = parse()

    val finalCounts = (0 until 256).foldLeft(initialCounts)(step)

    val result = finalCounts.values.sum

    println(s"Result 2: $result")
  }

  private def step(counts: Map[Int, Long], step: Int): Map[Int, Long] = {
    (0 to 8).map {
      case 8 => 8 -> counts.getOrElse(0, 0L)
      case 6 => 6 -> (counts.getOrElse(0, 0L) + counts.getOrElse(7, 0L))
      case x => x -> counts.getOrElse(x + 1, 0L)
    }.toMap
  }

  private def parse(): Map[Int, Long] = {
    val values = input.head.split(",").map(_.toInt)
    values
      .groupBy(x => x)
      .map { case (k, v) =>
        (k, v.length.toLong)
      }
  }
}

case object Day6 extends App {
  val input = Files.lines("2021/day6.txt")
  val problem = Day6(input)
  problem.solve1()
  problem.solve2()
}
