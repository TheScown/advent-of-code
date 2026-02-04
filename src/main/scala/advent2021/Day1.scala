package space.scown.adventofcode
package advent2021

import lib.{Files, Problem}

case class Day1(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val values = input.map(_.toInt)
    val pairs = values.sliding(2)
    val result = pairs.count {
      case Vector(a, b) => b > a
    }

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val values = input.map(_.toInt)
    val windows = values.sliding(3)
    val sums = windows.map(_.sum)
    val pairs = sums.sliding(2)
    val result = pairs.count {
      case Seq(a, b) => b > a
    }

    println(s"Result 2: $result")
  }
}

case object Day1 extends App {
  val input = Files.lines("2021/day1.txt")
  val problem = Day1(input)
  problem.solve1()
  problem.solve2()
}
