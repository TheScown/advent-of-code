package space.scown.adventofcode
package advent2018.problems

import advent2018.devicecode.Instruction
import advent2018.devicecode.Instruction._
import lib.{Files, Problem}

import scala.annotation.tailrec

case class Day16(input: Vector[String]) extends Problem {
  private type Function = (Vector[Int], Int, Int, Int) => Vector[Int]

  override def solve1(): Unit = {
    val (samples, _) = parse()

    val result = samples.count {
      case Sample(before, instruction, after) =>
        val results = functions.toVector
          .map(f => f(before, instruction.a, instruction.b, instruction.c))
        val matchingResults = results.filter(_ == after)
        matchingResults.size >= 3
    }

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val (samples, instructions) = parse()

    @tailrec
    def deduceInstructions(samples: Vector[Sample], functions: Set[Function], acc: Map[Int, Function]): Map[Int, Function] = {
      if (samples.isEmpty) acc
      else {
        val knownSamples = samples.map {
          case Sample(before, instruction, after) =>
            (instruction.opcode, functions.filter(f => f(before, instruction.a, instruction.b, instruction.c) == after))
        }.filter {
          case (_, fs) => fs.size == 1
        }.map {
          case (opcode, fs) => (opcode, fs.head)
        }

        val opcodesToRemove = knownSamples.map(_._1).toSet
        val functionsToRemove = knownSamples.map(_._2).toSet

        deduceInstructions(
          samples.filterNot(s => opcodesToRemove.contains(s.instruction.opcode)),
          functions.diff(functionsToRemove),
          acc ++ knownSamples
        )
      }
    }

    val instructionMap = deduceInstructions(samples, functions, Map())

    val result = instructions.foldLeft(Vector.fill(4)(0)) { (registers, instruction) =>
      val function = instructionMap(instruction.opcode)
      function(registers, instruction.a, instruction.b, instruction.c)
    }.head

    println(s"Result 2: $result")
  }

  private def parse(): (Vector[Sample], Vector[Instruction[Int]]) = {
    @tailrec
    def helper(
      remainder: Vector[String],
      samples: Vector[Sample]
    ): (Vector[Sample], Vector[Instruction[Int]]) = {
      if (remainder.tail.head.isEmpty) {
        (samples, remainder.tail.dropWhile(_.isEmpty).map(parseInstruction))
      }
      else {
        val pattern = "[A-Za-z]+:\\s+\\[(\\d+), (\\d+), (\\d+), (\\d+)]".r
        val (sampleLines, rest) = remainder.span(_.nonEmpty)
        val sample = sampleLines match {
          case Vector(before, instruction, after) => (before, after) match {
            case (pattern(a, b, c, d), pattern(e, f, g, h)) => Sample(
              Vector(a, b, c, d).map(_.toInt),
              parseInstruction(instruction),
              Vector(e, f, g, h).map(_.toInt)
            )
          }
        }

        helper(rest.tail, samples :+ sample)
      }
    }

    helper(input, Vector())
  }

  private def parseInstruction(s: String): Instruction[Int] = s.split(" ").map(_.toInt) match {
    case Array(opcode, a, b, c) => Instruction(opcode, a, b, c)
  }

  private case class Sample(before: Vector[Int], instruction: Instruction[Int], after: Vector[Int])

  private val functions = Set(
    addr _,
    addi _,
    mulr _,
    muli _,
    banr _,
    bani _,
    borr _,
    bori _,
    setr _,
    seti _,
    gtir _,
    gtri _,
    gtrr _,
    eqir _,
    eqri _,
    eqrr _
  )
}

case object Day16 extends App {
  val input = Files.lines("2018/day16.txt")
  val problem = Day16(input)
  problem.solve1()
  problem.solve2()
}
