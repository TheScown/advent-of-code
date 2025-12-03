package space.scown.adventofcode
package advent2025.problems

import lib.{Files, Problem}

case class Day3(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val banks = parse()

    val result = banks.map(computeJoltage(_, 2)).sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val banks = parse()

    val result = banks.map(computeJoltage(_, 12)).sum

    println(s"Result 2: $result")
  }

  private def computeJoltage(bank: Vector[Int], batteryCount: Int): Long = {
    (batteryCount - 1 to 0 by -1).foldLeft((0, 0L)) { case ((startIndex, acc), i) =>
      val (_, searchRange) = bank.splitAt(startIndex)
      val (finalSearchRange, _) = searchRange.splitAt(searchRange.size - i)
      val max = finalSearchRange.max
      val maxIndex = finalSearchRange.indexOf(max)

      (startIndex + maxIndex + 1, acc * 10 + max)
    }._2
  }

  private def parse(): Vector[Vector[Int]] = {
    input.map(_.toVector.map(_.asDigit))
  }
}

case object Day3 extends App {
  val input = Files.lines("2025/day3.txt")
  val problem = Day3(input)
  problem.solve1()
  problem.solve2()
}
