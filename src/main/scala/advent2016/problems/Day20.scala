package space.scown.adventofcode
package advent2016.problems

import lib.{Files, Problem}

import scala.annotation.tailrec

case class Day20(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val ranges = parse().sorted (Ordering.by[(Long, Long), Long](_._1))

    @tailrec
    def helper(x: Long): Long = {
      val isBlocked = ranges.exists { case (start, end) => x >= start && x <= end }

      if (!isBlocked) x
      else helper(x + 1)
    }

    val result = helper(0)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val ranges = parse().sorted (Ordering.by[(Long, Long), Long](_._1))

    @tailrec
    def helper(x: Long, count: Long, blacklist: Vector[(Long, Long)]): Long = {
      if (x > 4294967295L) count
      else {
        val (start, end) = blacklist.head

        if (start > x) {
          // we're in a clear window, whitelist up to the start of the next range
          helper(start, count + (start - x), blacklist)
        }
        else if (end < x) {
          // we're ahead of the range, we need to make sure we're not in the next one
          helper(x, count, blacklist.tail)
        }
        else {
          // We're inside the range, proceed to the end but don't whitelist
          helper(end + 1, count, blacklist.tail)
        }
      }
    }

    val result = helper(0, 0, ranges)

    println(s"Result 2: $result")
  }

  def parse(): Vector[(Long, Long)] = {
    val pattern = "(\\d+)-(\\d+)".r

    input.map {
      case pattern(start, end) => (start.toLong, end.toLong)
    }
  }
}

case object Day20 extends App {
  val input = Files.lines("2016/day20.txt")
  val problem = Day20(input)
  problem.solve1()
  problem.solve2()
}
