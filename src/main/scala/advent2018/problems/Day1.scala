package space.scown.adventofcode
package advent2018.problems

import lib.{Files, Problem}

import scala.annotation.tailrec

case class Day1(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val result = input.map(_.toInt).sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val frequencies = input.map(_.toInt).toList

    @tailrec
    def helper(remainingFrequencies: List[Int], currentTotal: Int, seen: Set[Int]): Int = {
      if (remainingFrequencies.isEmpty) helper(frequencies, currentTotal, seen)
      else {
        val next = remainingFrequencies.head
        val newTotal = currentTotal + next
        if (seen.contains(newTotal)) newTotal
        else helper(remainingFrequencies.tail, newTotal, seen + newTotal)
      }
    }

    val result = helper(frequencies, 0, Set())

    println(s"Result 2: $result")
  }
}

case object Day1 extends App {
  val input = Files.lines("2018/day1.txt")
  val problem = Day1(input)
  problem.solve1()
  problem.solve2()
}
