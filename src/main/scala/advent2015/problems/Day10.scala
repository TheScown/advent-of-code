package space.scown.adventofcode
package advent2015.problems

import lib.{Files, Problem, Timer}

import scala.annotation.tailrec

case class Day10(input: String) extends Problem {
  override def solve1(): Unit = {
    @tailrec
    def helper(input: String, remainingIterations: Int): String = {
      if (remainingIterations == 0) input
      else {
        println(remainingIterations)
        helper(processString(new StringBuilder(input)), remainingIterations - 1)
      }
    }

    val result = helper(input, 40).length
    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    @tailrec
    def helper(input: String, remainingIterations: Int): String = {
      if (remainingIterations == 0) input
      else {
        println(remainingIterations)
        helper(processString(new StringBuilder(input)), remainingIterations - 1)
      }
    }

    val result = helper(input, 50).length
    println(s"Result 2: $result")
  }

  @tailrec
  private def processString(input: StringBuilder, output: StringBuilder = new StringBuilder()): String = {
    if (input.isEmpty) output.toString()
    else {
      val prefix = input.takeWhile(c => c == input.head)
      processString(input.delete(0, prefix.length), output.append(s"${prefix.length}${prefix.charAt(0)}"))
    }
  }
}

case object Day10 extends App {
  val input = Files.lines("2015/day10.txt")(0)
  val problem = Day10(input)
  Timer.time(() => problem.solve1())
  Timer.time(() => problem.solve2())
}
