package space.scown.adventofcode
package advent2016.problems

import advent2016.assembunny.{Computer, Grammar, Instruction}
import lib.{Files, Problem, Timer}

case class Day23(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val instructions = parse()

    val result = run(instructions, Map(('a', 7), ('b', 0), ('c', 0), ('d', 0)))

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val instructions = parse()

    val result = run(instructions, Map(('a', 12), ('b', 0), ('c', 0), ('d', 0)))

    println(s"Result 2: $result")
  }

  def run(instructions: Vector[Instruction], registers: Map[Char, Int]): Int = {
    Computer.run(instructions, registers)
  }

  def parse(): Vector[Instruction] = {
    input.map { line =>
      Grammar.parse(Grammar.instruction, line) match {
        case Grammar.Success(instruction, _) => instruction
        case error@Grammar.Failure(_, _) => throw new IllegalArgumentException(s"$error")
        case Grammar.Error(msg, _) => throw new IllegalArgumentException(msg)
      }
    }
  }
}

case object Day23 extends App {
  val input = Files.lines("2016/day23.txt")
  val problem = Day23(input)
  Timer.time(() => problem.solve1())
  Timer.time(() => problem.solve2())
}
