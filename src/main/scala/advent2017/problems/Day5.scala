package space.scown.adventofcode
package advent2017.problems

import lib.{Files, Problem, Timer}

import scala.annotation.tailrec

case class Day5(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val program = input.map(_.toInt)

    @tailrec
    def helper(pc: Int, program: Vector[Int], jumpCount: Int): Int = {
      if (pc < 0 || pc >= program.size) jumpCount
      else helper(pc + program(pc), program.updated(pc, program(pc) + 1), jumpCount + 1)
    }

    val result = helper(0, program, 0)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val program = input.map(_.toInt)

    @tailrec
    def helper(pc: Int, program: Vector[Int], jumpCount: Int): Int = {
      if (pc < 0 || pc >= program.size) jumpCount
      else {
        val offset = program(pc)
        val newOffset = if (offset >= 3) offset - 1 else offset + 1
        helper(pc + offset, program.updated(pc, newOffset), jumpCount + 1)
      }
    }

    val result = helper(0, program, 0)

    println(s"Result 2: $result")
  }
}

case object Day5 extends App {
  val input = Files.lines("2017/day5.txt")
  val problem = Day5(input)
  Timer.time(() => problem.solve1())
  Timer.time(() => problem.solve2())
}
