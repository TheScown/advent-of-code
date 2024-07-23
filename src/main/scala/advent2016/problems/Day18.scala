package space.scown.adventofcode
package advent2016.problems

import lib.{Files, Problem}

import scala.annotation.tailrec

case class Day18(input: String) extends Problem {
  override def solve1(): Unit = {
    val result = solve(40)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
   val result = solve(400000)

    println(s"Result 2: $result")
  }

  private def solve(targetDepth: Int) = {
    @tailrec
    def helper(current: String, depth: Int, count: Int): Int = {
      if (depth == targetDepth) count
      else {
        val next = nextString(current)

        helper(next, depth + 1, count + safe(next))
      }
    }

    val result = helper(input, 1, safe(input))
    result
  }

  private def nextString(s: String): String = {
    val expanded = s".$s."
    s.indices.map { i =>
      val left = expanded(i)
      val centre = expanded(i + 1)
      val right = expanded(i + 2)

      if (
        (left == '^' && centre == '^' && right != '^') ||
          (left != '^' && centre == '^' && right == '^') ||
          (left == '^' && centre != '^' && right != '^') ||
          (left != '^' && centre != '^' && right == '^')
      ) '^' else '.'
    }.mkString("")
  }

  private def safe(s: String): Int = s.count(_ == '.')
}

case object Day18 extends App {
  val input = Files.lines("2016/day18.txt").head
  val problem = Day18(input)
  problem.solve1()
  problem.solve2()
}
