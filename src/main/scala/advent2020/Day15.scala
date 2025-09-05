package space.scown.adventofcode
package advent2020

import lib.{Files, Problem, Timer}

import scala.annotation.tailrec

case class Day15(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val initialNumbers = input.head.split(",").reverse.map(_.toInt).toList

    @tailrec
    def helper(numbers: List[Int]): Int = {
      if (numbers.size == 2020) numbers.head
      else {
        val index = numbers.tail.indexOf(numbers.head)

        if (index == -1) helper(0 :: numbers)
        else helper(index + 1 :: numbers)
      }
    }

    val result = helper(initialNumbers)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val initialNumbers = input.head.split(",").map(_.toInt)
    val indexMap = initialNumbers.zipWithIndex.toMap - initialNumbers.last

    @tailrec
    def helper(currentValue: Int, seen: Map[Int, Int], count: Int): Int = {
      if (count == 30_000_000 - 1) currentValue
      else {
        val index = seen.getOrElse(currentValue, -1)

        if (index == -1) helper(0, seen + (currentValue -> count), count + 1)
        else helper(count - index, seen + (currentValue -> count), count + 1)
      }
    }

    val result = helper(initialNumbers.last, indexMap, indexMap.size)

    println(s"Result 2: $result")
  }
}

case object Day15 extends App {
  val input = Files.lines("2020/day15.txt")
  val problem = Day15(input)
  problem.solve1()
  Timer.time(() => problem.solve2())
}
