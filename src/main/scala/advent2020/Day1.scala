package space.scown.adventofcode
package advent2020

import lib.{Files, Problem}

case class Day1(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val matchingPair = input
      .map(_.toInt)
      .combinations(2)
      .find(pair => pair.sum == 2020)
      .get

    val result = matchingPair.product

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val matchingPair = input
      .map(_.toInt)
      .combinations(3)
      .find(pair => pair.sum == 2020)
      .get

    val result = matchingPair.product

    println(s"Result 2: $result")
  }
}

case object Day1 extends App {
  val input = Files.lines("2020/day1.txt")
  val problem = Day1(input)
  problem.solve1()
  problem.solve2()
}
