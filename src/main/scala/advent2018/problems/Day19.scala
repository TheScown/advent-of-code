package space.scown.adventofcode
package advent2018.problems

import advent2018.devicecode.Instruction._
import advent2018.devicecode._
import lib.{Files, Integers, Problem, Timer}

import scala.annotation.tailrec

case class Day19(input: Vector[String]) extends Problem {
  type Func = (Vector[Int], Int, Int, Int) => Vector[Int]

  override def solve1(): Unit = {
    val (instructions, ipRegister) = parse()

    val result = run(instructions, Vector.fill(6)(0), ipRegister)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val (instructions, ipRegister) = parse()

    val result = run(instructions, 1 +: Vector.fill(5)(0), ipRegister)

    println(s"Result 2: $result")
  }

  private def run(instructions: Vector[Instruction[Func]], registers: Vector[Int], ipRegister: Int): Int = {
    @tailrec
    def helper(registers: Vector[Int], ip: Int): Int = {
      if (ip < 0 || ip >= instructions.size) registers(0)
      else if (ip == 1) {
        // Do optimised thing here
        Integers.factors(registers.max).sum
      }
      else {
        val afterIpUpdate = registers.updated(ipRegister, ip)
        val instruction = instructions(ip)
        val updatedRegisters = instruction.opcode(afterIpUpdate, instruction.a, instruction.b, instruction.c)

        helper(updatedRegisters, updatedRegisters(ipRegister) + 1)
      }
    }

    helper(registers, 0)
  }

  private def parse(): (Vector[Instruction[Func]], Int) = {
    val ipDirective = input.head
    val instructionStrings = input.tail

    val ipRegister = ipDirective.split(" ").last.toInt

    val instructionPattern = "([a-z]{4}) (\\d+) (\\d+) (\\d+)".r

    val instructions = instructionStrings.map {
      case instructionPattern(op, a, b, c) => op match {
        case "addr" => Instruction[Func](addr, a.toInt, b.toInt, c.toInt)
        case "addi" => Instruction[Func](addi, a.toInt, b.toInt, c.toInt)
        case "mulr" => Instruction[Func](mulr, a.toInt, b.toInt, c.toInt)
        case "muli" => Instruction[Func](muli, a.toInt, b.toInt, c.toInt)
        case "banr" => Instruction[Func](banr, a.toInt, b.toInt, c.toInt)
        case "bani" => Instruction[Func](bani, a.toInt, b.toInt, c.toInt)
        case "borr" => Instruction[Func](borr, a.toInt, b.toInt, c.toInt)
        case "bori" => Instruction[Func](bori, a.toInt, b.toInt, c.toInt)
        case "setr" => Instruction[Func](setr, a.toInt, b.toInt, c.toInt)
        case "seti" => Instruction[Func](seti, a.toInt, b.toInt, c.toInt)
        case "gtir" => Instruction[Func](gtir, a.toInt, b.toInt, c.toInt)
        case "gtri" => Instruction[Func](gtri, a.toInt, b.toInt, c.toInt)
        case "gtrr" => Instruction[Func](gtrr, a.toInt, b.toInt, c.toInt)
        case "eqir" => Instruction[Func](eqir, a.toInt, b.toInt, c.toInt)
        case "eqri" => Instruction[Func](eqri, a.toInt, b.toInt, c.toInt)
        case "eqrr" => Instruction[Func](eqrr, a.toInt, b.toInt, c.toInt)
      }
    }

    (instructions, ipRegister)
  }
}

case object Day19 extends App {
  val input = Files.lines("2018/day19.txt")
  val problem = Day19(input)
  problem.solve1()
  Timer.time(() => problem.solve2())
}
