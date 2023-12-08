package space.scown.adventofcode2019
package problems

import intcode.{IntcodeComputer, IntcodeProgram}
import lib.{Files, Problem}

case class Day5(lines: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val program = IntcodeProgram.fromLines(lines)
    val computer = IntcodeComputer(program, () => 1, println)

    computer.execute()

    println(s"Result 1: See STDOUT")
  }

  override def solve2(): Unit = {

  }

}

object Day5 {
  def main(args: Array[String]): Unit = {
    val value = Files.lines("day5.txt")
    Day5(value).solve1()
    Day5(value).solve2()
  }

}

