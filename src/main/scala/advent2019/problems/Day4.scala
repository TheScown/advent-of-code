package space.scown.adventofcode
package advent2019.problems

import lib.{Files, Integers, Problem}

import scala.annotation.tailrec

case class Day4(input: String) extends Problem {
  override def solve1(): Unit = {
    val (start, end) = parse()

    val result = initialList(start, end).size

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val (start, end) = parse()

    val result = initialList(start, end)
      .count { x =>
        def s = x.toString

        @tailrec
        def helper(index: Int, currentDigit: Char, consecutive: Int): Boolean = {
          if (index == s.length) consecutive == 2
          else {
            val next = s(index)
            if (next == currentDigit) helper(index + 1, currentDigit, consecutive + 1)
            else if (consecutive == 2) true
            else helper(index + 1, next, 1)
          }
        }

        helper(1, s(0), 1)
      }

    println(s"Result 2: $result")
  }

  private def initialList(start: Int, end: Int): LazyList[Int] = {
    Integers.naturalNumbers[Int]
      .slice(start - 1, end)
      .filter { x =>
        x.toString.sliding(2).exists(w => w(0) == w(1))
      }
      .filter { x =>
        x.toString.sorted == x.toString
      }
  }

  private def parse(): (Int, Int) = {
    val parts = input.split("-").map(_.toInt)
    (parts(0), parts(1))
  }
}

case object Day4 extends App {
  val input = Files.lines("2019/day4.txt").head
  val problem = Day4(input)
  problem.solve1()
  problem.solve2()
}
