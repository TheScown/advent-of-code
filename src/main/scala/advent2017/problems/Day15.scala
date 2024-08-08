package space.scown.adventofcode
package advent2017.problems

import lib.{Files, Problem, Timer}

import scala.annotation.tailrec

case class Day15(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val result = parse() match {
      case Vector(a, b) =>
        @tailrec
        def helper(currentA: Long, currentB: Long, count: Int, iterations: Int): Int = {
          if (iterations == 40_000_000) count
          else {
            val nextA = (currentA * 16807) % 2147483647
            val nextB = (currentB * 48271) % 2147483647
            val matches = (nextA & 0xFFFF) == (nextB & 0xFFFF)

            helper(nextA, nextB, count + (if (matches) 1 else 0), iterations + 1)
          }
        }

        helper(a, b, 0, 0)
    }

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val result = parse() match {
      case Vector(a, b) =>
        @tailrec
        def helper(currentA: Long, currentB: Long, count: Int, iterations: Int, tempA: Vector[Long], tempB: Vector[Long]): Int = {
          if (iterations == 5_000_000) count
          else {
            val nextA = (currentA * 16807) % 2147483647
            val nextB = (currentB * 48271) % 2147483647

            val asToCheck = if (nextA % 4 == 0) {
              tempA :+ nextA
            } else tempA

            val bsToCheck = if (nextB % 8 == 0) {
              tempB :+ nextB
            } else tempB

            if (asToCheck.nonEmpty && bsToCheck.nonEmpty) {
              val matches = (asToCheck.head & 0xFFFF) == (bsToCheck.head & 0xFFFF)
              helper(nextA, nextB, count + (if (matches) 1 else 0), iterations + 1, asToCheck.tail, bsToCheck.tail)
            }
            else helper(nextA, nextB, count, iterations, asToCheck, bsToCheck)
          }
        }

        helper(a, b, 0, 0, Vector(), Vector())
    }

    println(s"Result 2: $result")
  }

  private def parse(): Vector[Long] = {
    val pattern = ".*?(\\d+)".r

    input.map {
      case pattern(x) => x.toLong
    }
  }
}

case object Day15 extends App {
  val input = Files.lines("2017/day15.txt")
  val problem = Day15(input)
  Timer.time(() => problem.solve1())
  Timer.time(() => problem.solve2())
}
