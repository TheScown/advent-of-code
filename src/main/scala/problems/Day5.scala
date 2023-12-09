package space.scown.adventofcode2019
package problems

import intcode.{IntcodeComputer, IntcodeProgram}
import lib.{Files, Problem}

case class Day5(lines: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val program = IntcodeProgram.fromLines(lines)
    val computer = IntcodeComputer(program)

    val (_, output) = computer.execute(LazyList(1))

    // Should be 9025675
    println(s"Result 1: ${output.last}")
  }

  override def solve2(): Unit = {
    val program = IntcodeProgram.fromLines(lines)
    val computer = IntcodeComputer(program)

    val (_, output) = computer.execute(LazyList(5))

    // Should be 11981754
    println(s"Result 2: ${output.head}")
  }

}

object Day5 {
  def main(args: Array[String]): Unit = {
    val value = Files.lines("day5.txt")
    Day5(value).solve1()
    Day5(value).solve2()
  }

}

