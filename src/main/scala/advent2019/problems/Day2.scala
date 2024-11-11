package space.scown.adventofcode
package advent2019.problems

import advent2019.intcode.{IntcodeComputer, IntcodeProgram, Termination}
import lib.{Files, Integers, Problem}

import scala.math.Numeric.LongIsIntegral

case class Day2(lines: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val program = IntcodeProgram.fromLines(lines)
    val updatedProgram = program.updated(1, 12L).updated(2, 2L)
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

    def intList: LazyList[Long] = Integers.naturalNumbers(LongIsIntegral).map(_ - 1).take(100)

    val pairs = intList.flatMap(i => intList.map(j => (i, j)))

    val resultPair = pairs.find {
      case (i, j) =>
        val updatedProgram = program.updated(1, i).updated(2, j)
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

case object Day2 extends App {
  val value = Files.lines("2019/day2.txt")
  val problem = Day2(value)
  problem.solve1()
  problem.solve2()
}

