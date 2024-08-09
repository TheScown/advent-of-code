package space.scown.adventofcode
package advent2017.problems

import lib.{Files, Problem, Timer}

import scala.annotation.tailrec

case class Day17(input: String) extends Problem {
  override def solve1(): Unit = {
    val steps = input.toInt

    @tailrec
    def helper(sequence: Vector[Int], index: Int, lastValue: Int): Int = {
      if (lastValue == 2017) sequence(index + 1)
      else {
        val newIndex = (index + steps) % sequence.size
        val (before, after) = sequence.splitAt(newIndex + 1)
        helper(before ++ ((lastValue + 1) +: after), newIndex + 1, lastValue + 1)
      }
    }

    val result = helper(Vector(0), 0, 0)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val steps = input.toInt

    @tailrec
    def helper(index: Int, size: Int, lastSeenValue: Int): Int = {
      if (size == 50_000_000) lastSeenValue
      else {
        val newIndex = (index + steps) % size

        if (newIndex == 0) helper(newIndex + 1, size + 1, size)
        else helper(newIndex + 1, size + 1, lastSeenValue)
      }
    }

    val result = helper(0, 1, 0)

    println(s"Result 2: $result")
  }
}

case object Day17 extends App {
  val input = Files.lines("2017/day17.txt")
  val problem = Day17(input.head)
  problem.solve1()
  problem.solve2()
}
