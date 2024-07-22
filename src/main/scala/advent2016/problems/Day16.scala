package space.scown.adventofcode
package advent2016.problems

import lib.{Files, Problem, Timer}

import scala.annotation.tailrec

case class Day16(input: String) extends Problem {
  override def solve1(): Unit = {
    val digits = input.toCharArray.toVector

    val targetSize = 272

    val result = solve(digits, targetSize)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val digits = input.toCharArray.toVector

    val targetSize = 35651584

    val result = solve(digits, targetSize)

    println(s"Result 2: $result")
  }

  private def solve(digits: Vector[Char], targetSize: Int) = {
    @tailrec
    def expand(current: Vector[Char]): Vector[Char] = {
      if (current.size >= targetSize) current.take(targetSize)
      else {
        val b = current.reverse.map(x => if (x == 1) '0' else '1')
        expand((current :+ '0') ++ b)
      }
    }

    val expanded = expand(digits)

    @tailrec
    def checksum(current: Vector[Char]): String = {
      if (current.size % 2 == 1) current.mkString("")
      else {
        checksum(current.grouped(2).map { p =>
          if (p(0) == p(1)) '1' else '0'
        }.toVector)
      }
    }

    val result = checksum(expanded)
    result
  }
}

case object Day16 extends App {
  val input = Files.lines("2016/day16.txt").head
  val problem = Day16(input)
  Timer.time(() => problem.solve1())
  Timer.time(() => problem.solve2())
}
