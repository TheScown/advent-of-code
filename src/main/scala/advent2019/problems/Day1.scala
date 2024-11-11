package space.scown.adventofcode
package advent2019.problems

import lib.{Files, Problem}

import scala.annotation.tailrec

case class Day1(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val result = input.map(_.toInt).map(fuelRequired).sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    @tailrec
    def helper(mass: Int, acc: Int): Int = {
      if (mass < 0) acc
      else {
        helper(fuelRequired(mass), acc + mass)
      }
    }

    val result = input.map(_.toInt).map(fuelRequired).map(helper(_, 0)).sum

    println(s"Result 2: $result")
  }

  private def fuelRequired(mass: Int): Int = mass / 3 - 2
}

case object Day1 extends App {
  val input = Files.lines("2019/day1.txt")
  val problem = Day1(input)
  problem.solve1()
  problem.solve2()
}
