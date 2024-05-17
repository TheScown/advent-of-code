package space.scown.adventofcode
package advent2015.problems

import lib.{Files, Problem}

import scala.annotation.tailrec

case class Day1(input: String) extends Problem {
  override def solve1(): Unit = {
    val result = input.map {
      case '(' => 1
      case ')' => -1
    }.sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    @tailrec
    def helper(floor: Int, index: Int = 0): Int = {
      if (floor == -1) index
      else {
        val floorDelta = input.charAt(index) match {
          case '(' => 1
          case ')' => -1
        }

        helper(floor + floorDelta, index + 1)
      }
    }

    val result = helper(0)

    println(s"Result 2: $result")
  }
}

case object Day1 extends App {
  val problem = Day1(Files.lines("2015/day1.txt").mkString(""))

  problem.solve1()
  problem.solve2()
}
