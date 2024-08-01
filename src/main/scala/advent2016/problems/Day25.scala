package space.scown.adventofcode
package advent2016.problems

import advent2016.assembunny.{Computer, Grammar, Instruction}
import lib.{Files, Integers, Problem}

case class Day25(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val instructions = parse()

    val result = Integers.naturalNumbers.find { i =>
      val list = run(instructions, Map(('a', i), ('b', 0), ('c', 0), ('d', 0))).take(8)
      list.toVector == Vector(0, 1, 0, 1, 0, 1, 0, 1)
    }.get

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    println("Result 2: Transmit the signal!")
  }

  def run(instructions: Vector[Instruction], registers: Map[Char, Int]): LazyList[Int] = {
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

case object Day25 extends App {
  val input = Files.lines("2016/day25.txt")
  val problem = Day25(input)
  problem.solve1()
  problem.solve2()
}
