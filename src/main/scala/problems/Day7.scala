package space.scown.adventofcode2019
package problems

import intcode.{IntcodeComputer, IntcodeProgram, Output, RequiresInput}
import lib.{Files, Problem}

import scala.annotation.tailrec

case class Day7(lines: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val program = IntcodeProgram.fromLines(lines)
    val permutations = (0 until 5).permutations

    val result = permutations.map { phases =>
      @tailrec
      def helper(remainingPhases: IndexedSeq[Int], nextInput: Long = 0): Long = {
        if (remainingPhases.isEmpty) return nextInput

        val phase = remainingPhases.head

        val computer = IntcodeComputer(program)

        val result = Seq(phase, nextInput).foldLeft(computer.execute()) { (output, value) => output match {
          case RequiresInput(_, continue) => continue(value)
        } }

        helper(remainingPhases.tail, result.outputs.last)
      }

      helper(phases)
    }.max

    // Should be 21760
    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val program = IntcodeProgram.fromLines(lines)
    val permutations = (5 to 9).permutations

    val result = permutations.map { phases =>
//      println(s"Phases $phases")

      val computers = phases.map { phase =>
        IntcodeComputer(program).execute() match {
          case RequiresInput(_, continue) => continue(phase)
        }
      }

      @tailrec
      def cycleComputers(nextInput: Long, computers: Seq[Output], nextComputers: Seq[Output]): Long = {
        if (computers.isEmpty) {
          if (nextComputers.last.terminal) nextComputers.last.outputs.last
          else cycleComputers(nextComputers.last.outputs.last, nextComputers, Vector())
        }
        else {
          val nextOutput = computers.head match {
            case RequiresInput(_, continue) => continue(nextInput)
          }

          cycleComputers(nextOutput.outputs.last, computers.tail, nextComputers :+ nextOutput)
        }
      }

      cycleComputers(0, computers, Vector())
    }.max

    // Should be 69816958
    println(s"Result 2: $result")
  }
}

object Day7 {
  def main(args: Array[String]): Unit = {
    val value = Files.lines("day7.txt")
    Day7(value).solve1()
    Day7(value).solve2()
  }

}
