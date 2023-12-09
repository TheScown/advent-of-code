package space.scown.adventofcode2019
package problems

import intcode.{IntcodeComputer, IntcodeProgram}
import lib.{Files, Problem}

case class Day9(lines: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val program = IntcodeProgram.fromLines(lines)
    val computer = IntcodeComputer(program)

    val output = computer.execute(LazyList(1))
      .map(_._2)
      .filter(_.isDefined)
      .map(_.get)

    // Should be 3533056970
    println(s"Result 1: ${output.last}")
  }

  override def solve2(): Unit = {
    val program = IntcodeProgram.fromLines(lines)
    val computer = IntcodeComputer(program)

    val output = computer.execute(LazyList(2))
      .map(_._2)
      .filter(_.isDefined)
      .map(_.get)

    // Should be 72852
    println(s"Result 2: ${output.last}")
  }
}

object Day9 {
  def main(args: Array[String]): Unit = {
    val value = Files.lines("day9.txt")
    Day9(value).solve1()
    Day9(value).solve2()
  }

}
