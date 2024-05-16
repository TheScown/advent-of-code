package space.scown.adventofcode
package advent2019.problems

import advent2019.intcode.{IntcodeComputer, IntcodeProgram, Termination}
import lib.{Files, Problem}

case class Day2(lines: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val program = IntcodeProgram.fromLines(lines)
    val updatedProgram = program.updated(1, 12.toLong).updated(2, 2.toLong)
    val computer = IntcodeComputer(updatedProgram)

    computer.execute() match {
      case Termination(_, memory) =>
        val result = memory(0)

        // Should be 4570637
        println(s"Result 1: $result")
    }
  }

  override def solve2(): Unit = {
    val program = IntcodeProgram.fromLines(lines)

    val inputs = for {
      i <- 0 to 99
      j <- 0 to 99
    } yield (i, j)

    val resultPair = inputs.find {
      case (i, j) =>
        val updatedProgram = program.updated(1, i.toLong).updated(2, j.toLong)
        val computer = IntcodeComputer(updatedProgram)
        computer.execute() match {
          case Termination(_, memory) => memory(0) == 19690720
        }
    }.get

    val result = 100 * resultPair._1 + resultPair._2

    // Should be 5485
    println(s"Result 2: $result")
  }

}

object Day2 {
  def main(args: Array[String]): Unit = {
    val value = Files.lines("2019/day2.txt")
    Day2(value).solve1()
    Day2(value).solve2()
  }

}

