package space.scown.adventofcode2019
package problems

import intcode.{IntcodeComputer, IntcodeProgram}
import lib.{Files, Problem}

case class Day2(lines: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val program = IntcodeProgram.fromLines(lines)
    val updatedProgram = program.updated(1, 12).updated(2, 2)
    val computer = IntcodeComputer(updatedProgram)

    val finalState = computer.execute()
    val result = finalState(0)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val program = IntcodeProgram.fromLines(lines)

    val inputs = for {
      i <- 0 to 99
      j <- 0 to 99
    } yield (i, j)

    val resultPair = inputs.find {
      case (i, j) =>
        val updatedProgram = program.updated(1, i).updated(2, j)
        val computer = IntcodeComputer(updatedProgram)
        val finalState = computer.execute()
        finalState(0) == 19690720
    }.get

    val result = 100 * resultPair._1 + resultPair._2

    println(s"Result 2: $result")
  }

}

object Day2 {
  def main(args: Array[String]): Unit = {
    val value = Files.lines("day2.txt")
    Day2(value).solve1()
    Day2(value).solve2()
  }

}

