package space.scown.adventofcode
package advent2019.problems

import advent2019.intcode.{AsciiAdapter, IntcodeComputer, IntcodeProgram}
import lib.{Files, Problem}

import scala.annotation.tailrec
import scala.io.StdIn.readLine

case class Day21(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    // Determined by inspection
    val result = solve(Seq(
      "NOT A T",
      "NOT B J",
      "OR J T",
      "NOT C J",
      "OR T J",
      "AND D J",
      "WALK"
    ))

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    // Determined by inspection
    val result = solve(Seq(
      "NOT A T",
      "NOT B J",
      "OR J T",
      "NOT C J",
      "OR J T",
      "AND D T",
      "NOT E J",
      "AND H J",
      "OR E J",
      "AND T J",
      "RUN"
    ))

    println(s"Result 2: $result")
  }

  private def solve(script: Seq[String]) = {
    val program = IntcodeProgram.fromLines(input)
    val computer = IntcodeComputer(program)
    val asciiComputer = AsciiAdapter(computer.execute())

    val finalComputer = script.foldLeft(asciiComputer) { (computer, line) =>
      computer.sendString(s"$line\n")
    }

    val result = finalComputer.outputs.last
    result
  }

  @tailrec
  private def interactivePrompt(asciiComputer: AsciiAdapter): Long = {
    print(asciiComputer.stringOutput)

    val nextLine = readLine()

    val newComputer = asciiComputer.sendString(s"$nextLine\n")

    if (nextLine == "WALK" || nextLine == "RUN") {
      val output = newComputer.stringOutput

      if (output.contains("Didn't make it across")) {
        print(output)
        println("Reboot")
        interactivePrompt(newComputer)
      }
      else asciiComputer.output.outputs.last
    }
    else interactivePrompt(newComputer)
  }
}

case object Day21 extends App {
  val input = Files.lines("2019/day21.txt")
  val problem = Day21(input)
  problem.solve1()
  problem.solve2()
}
