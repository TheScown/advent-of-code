package space.scown.adventofcode
package advent2018.problems

import lib.{Files, Problem}

import scala.annotation.tailrec

case class Day16(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val (samples, _) = parse()

    val result = samples.count {
      case Sample(before, instruction, after) =>
        val results = functions
          .map(f => f(before, instruction.a, instruction.b, instruction.c))
        val matchingResults = results.filter(_ == after)
        matchingResults.size >= 3
    }

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {

  }

  private def parse(): (Vector[Sample], Vector[Instruction]) = {
    @tailrec
    def helper(
      remainder: Vector[String],
      samples: Vector[Sample]
    ): (Vector[Sample], Vector[Instruction]) = {
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

  private def parseInstruction(s: String): Instruction = s.split(" ").map(_.toInt) match {
    case Array(opcode, a, b, c) => Instruction(opcode, a, b, c)
  }

  private case class Instruction(opcode: Int, a: Int, b: Int, c: Int)
  private case class Sample(before: Vector[Int], instruction: Instruction, after: Vector[Int])

  private def addr(registers: Vector[Int], a: Int, b: Int, c: Int): Vector[Int] = {
    registers.updated(c, registers(a) + registers(b))
  }

  private def addi(registers: Vector[Int], a: Int, b: Int, c: Int): Vector[Int] = {
    registers.updated(c, registers(a) + b)
  }

  private def mulr(registers: Vector[Int], a: Int, b: Int, c: Int): Vector[Int] = {
    registers.updated(c, registers(a) * registers(b))
  }

  private def muli(registers: Vector[Int], a: Int, b: Int, c: Int): Vector[Int] = {
    registers.updated(c, registers(a) * b)
  }

  private def banr(registers: Vector[Int], a: Int, b: Int, c: Int): Vector[Int] = {
    registers.updated(c, registers(a) & registers(b))
  }

  private def bani(registers: Vector[Int], a: Int, b: Int, c: Int): Vector[Int] = {
    registers.updated(c, registers(a) & b)
  }

  private def borr(registers: Vector[Int], a: Int, b: Int, c: Int): Vector[Int] = {
    registers.updated(c, registers(a) | registers(b))
  }

  private def bori(registers: Vector[Int], a: Int, b: Int, c: Int): Vector[Int] = {
    registers.updated(c, registers(a) | b)
  }

  private def setr(registers: Vector[Int], a: Int, b: Int, c: Int): Vector[Int] = {
    registers.updated(c, registers(a))
  }

  private def seti(registers: Vector[Int], a: Int, b: Int, c: Int): Vector[Int] = {
    registers.updated(c, a)
  }

  private def gtir(registers: Vector[Int], a: Int, b: Int, c: Int): Vector[Int] = {
    registers.updated(c, if (a > registers(b)) 1 else 0)
  }

  private def gtri(registers: Vector[Int], a: Int, b: Int, c: Int): Vector[Int] = {
    registers.updated(c, if (registers(a) > b) 1 else 0)
  }

  private def gtrr(registers: Vector[Int], a: Int, b: Int, c: Int): Vector[Int] = {
    registers.updated(c, if (registers(a) > registers(b)) 1 else 0)
  }

  private def eqir(registers: Vector[Int], a: Int, b: Int, c: Int): Vector[Int] = {
    registers.updated(c, if (a == registers(b)) 1 else 0)
  }

  private def eqri(registers: Vector[Int], a: Int, b: Int, c: Int): Vector[Int] = {
    registers.updated(c, if (registers(a) == b) 1 else 0)
  }

  private def eqrr(registers: Vector[Int], a: Int, b: Int, c: Int): Vector[Int] = {
    registers.updated(c, if (registers(a) == registers(b)) 1 else 0)
  }

  private val functions = Vector(
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
