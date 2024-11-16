package space.scown.adventofcode
package advent2019.problems

import advent2019.intcode.{IntcodeComputer, IntcodeProgram, RequiresInput, Termination}
import lib.{Files, Problem}

case class Day5(lines: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val result = (computer.execute() match {
      case RequiresInput(_, continue) => continue(1L)
    }) match {
      case Termination(outputs, _) => outputs.find(x => x != 0).get
    }

    // Should be 9025675
    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val result = (computer.execute() match {
      case RequiresInput(_, continue) => continue(5L)
    }) match {
      case Termination(outputs, _) => outputs.find(x => x != 0).get
    }

    // Should be 11981754
    println(s"Result 2: $result")
  }

  private def computer = {
    val program = IntcodeProgram.fromLines(lines)
    IntcodeComputer(program)
  }
}

object Day5 extends App {
  val value = Files.lines("2019/day5.txt")
  val problem = Day5(value)
  problem.solve1()
  problem.solve2()
}

