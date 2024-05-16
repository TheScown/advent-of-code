package space.scown.adventofcode
package advent2019.problems

import advent2019.intcode.{IntcodeComputer, IntcodeProgram, RequiresInput}
import lib.{Files, Problem}

case class Day9(lines: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val program = IntcodeProgram.fromLines(lines)
    val computer = IntcodeComputer(program)

    val output = computer.execute() match {
      case RequiresInput(_, continue) => continue(1)
    }

    // Should be 3533056970
    println(s"Result 1: ${output.outputs.last}")
  }

  override def solve2(): Unit = {
    val program = IntcodeProgram.fromLines(lines)
    val computer = IntcodeComputer(program)

    val output = computer.execute() match {
      case RequiresInput(_, continue) => continue(2)
    }

    // Should be 72852
    println(s"Result 2: ${output.outputs.last}")
  }
}

object Day9 {
  def main(args: Array[String]): Unit = {
    val value = Files.lines("2019/day9.txt")
    Day9(value).solve1()
    Day9(value).solve2()
  }

}
