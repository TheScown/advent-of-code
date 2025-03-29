package space.scown.adventofcode
package advent2019.problems

import advent2019.intcode.{IntcodeComputer, RequiresInput, Termination}
import intcodeassembler.Assembler
import lib.{Files, Problem, Timer}

import scala.util.parsing.input.PagedSeq

case class Day1Intcode(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val result = solve("2019/day1-1.ia")

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val result = solve("2019/day1-2.ia")

    println(s"Result 1: $result")
  }

  private def solve(assemblyFile: String): Long = {
    val parsedInput = input.map(_.toLong)

    val assembly = Files.pagedSequence(assemblyFile)
    val program = Assembler(PagedSeq.fromIterable(assembly)).assembled
    val computer = IntcodeComputer(program.toVector)

    val finalState = parsedInput.foldLeft(computer.execute()) { (output, i) =>
      output match {
        case RequiresInput(_, continue) => continue(i)
        case Termination(_, _) => throw new IllegalStateException("Program terminated")
      }
    }

    finalState.outputs.last
  }
}

case object Day1Intcode extends App {
  val input = Files.lines("2019/day1.txt")
  val problem = Day1Intcode(input)
  Timer.time(() => problem.solve1())
  Timer.time(() => problem.solve2())
}
