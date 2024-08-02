package space.scown.adventofcode
package advent2017.problems

import lib.{Files, Problem}

case class Day1(input: String) extends Problem {
  override def solve1(): Unit = {
    val cycle = input + input.head
    val result = cycle.toVector
      .map(_.asDigit)
      .sliding(2)
      .filter {
        case Vector(a, b) => a == b
      }
      .map(_.head)
      .sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val integers = input.toVector.map(_.asDigit)
    val (first, second) = integers.splitAt(integers.size / 2)

    val result = first.zip(second)
      .filter {
        case (a, b) => a == b
      }
      .map(_._1)
      .sum * 2

    println(s"Result 2: $result")
  }
}

case object Day1 extends App {
  val input = Files.lines("2017/day1.txt").head
  val problem = Day1(input)
  problem.solve1()
  problem.solve2()
}
