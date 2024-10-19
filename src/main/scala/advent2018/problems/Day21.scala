package space.scown.adventofcode
package advent2018.problems

import advent2018.devicecode.Instruction
import lib.{Files, Problem, Timer}

import scala.annotation.tailrec

case class Day21(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val (instructions, ipRegister) = Instruction.parse(input)

    @tailrec
    def helper(registers: Vector[Int], ip: Int): Int = {
      if (ip < 0 || ip >= instructions.size) registers(0)
      else {
        if (ip == 28) {
          // By hand analysis, the program will terminate here if R5 == R0 (the input we want to find)
          return registers(5)
        }

        val afterIpUpdate = registers.updated(ipRegister, ip)
        val instruction = instructions(ip)
        val updatedRegisters = instruction.opcode(afterIpUpdate, instruction.a, instruction.b, instruction.c)

        helper(updatedRegisters, updatedRegisters(ipRegister) + 1)
      }
    }

    val result = helper(Vector.fill(6)(0), 0)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val (instructions, ipRegister) = Instruction.parse(input)

    @tailrec
    def helper(registers: Vector[Int], ip: Int, seen: Set[Int], lastValue: Int): Int = {
      if (ip < 0 || ip >= instructions.size) registers(0)
      else {
        if (ip == 28) {
          if (seen.contains(registers(5))) return lastValue
        }

        val afterIpUpdate = registers.updated(ipRegister, ip)
        val instruction = instructions(ip)
        val updatedRegisters = instruction.opcode(afterIpUpdate, instruction.a, instruction.b, instruction.c)

        val updatedSeen = if (ip == 28) seen + registers(5) else seen
        val newLastValue = if (ip == 28) registers(5) else lastValue

        helper(updatedRegisters, updatedRegisters(ipRegister) + 1, updatedSeen, newLastValue)
      }
    }

    val result = helper(Vector.fill(6)(0), 0, Set(), 0)

    println(s"Result 2: $result")
  }
}

case object Day21 extends App {
  val input = Files.lines("2018/day21.txt")
  val problem = Day21(input)
  problem.solve1()
  Timer.time(() => problem.solve2())
}
