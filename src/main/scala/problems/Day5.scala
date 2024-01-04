package space.scown.adventofcode2019
package problems

import intcode.{IntcodeComputer, IntcodeProgram, RequiresInput, Termination}
import lib.{Files, Problem}

case class Day5(lines: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val program = IntcodeProgram.fromLines(lines)
    val computer = IntcodeComputer(program)

    val output = (computer.execute() match {
      case RequiresInput(_, continue) => continue(1L)
    }) match {
      case Termination(outputs, _) => outputs.find(x => x != 0)
    }

    // Should be 9025675
    println(s"Result 1: ${output.get}")
  }

  override def solve2(): Unit = {
    val program = IntcodeProgram.fromLines(lines)
    val computer = IntcodeComputer(program)

    val output = (computer.execute() match {
      case RequiresInput(_, continue) => continue(5L)
    }) match {
      case Termination(outputs, _) => outputs.find(x => x != 0)
    }

    // Should be 11981754
    println(s"Result 2: ${output.get}")
  }

}

object Day5 {
  def main(args: Array[String]): Unit = {
    val value = Files.lines("day5.txt")
    Day5(value).solve1()
    Day5(value).solve2()
  }

}

