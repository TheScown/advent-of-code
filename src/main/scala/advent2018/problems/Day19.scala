package space.scown.adventofcode
package advent2018.problems

import advent2018.devicecode._
import lib.{Files, Integers, Problem, Timer}

import scala.annotation.tailrec

case class Day19(input: Vector[String]) extends Problem {
  type Func = (Vector[Int], Int, Int, Int) => Vector[Int]

  override def solve1(): Unit = {
    val (instructions, ipRegister) = Instruction.parse(input)

    val result = run(instructions, Vector.fill(6)(0), ipRegister)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val (instructions, ipRegister) = Instruction.parse(input)

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

}

case object Day19 extends App {
  val input = Files.lines("2018/day19.txt")
  val problem = Day19(input)
  problem.solve1()
  Timer.time(() => problem.solve2())
}
