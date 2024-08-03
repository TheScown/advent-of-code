package space.scown.adventofcode
package advent2017.problems

import lib.{Files, Problem}

case class Day2(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val result = input.map { line =>
      val values = line.split("\\s").map(_.toInt)
      values.max - values.min
    }.sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val result = input.map { line =>
      val values = line.split("\\s").map(_.toInt)
      values.combinations(2)
        .map(_.sorted)
        .find(a => a(1) % a(0) == 0)
        .map(a => a(1) / a(0)).get
    }.sum

    println(s"Result 2: $result")
  }
}

case object Day2 extends App {
  val input = Files.lines("2017/day2.txt")
  val problem = Day2(input)
  problem.solve1()
  problem.solve2()
}
