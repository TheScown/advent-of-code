package space.scown.adventofcode
package advent2015.problems

import lib.{Complex, Files, Problem}

import scala.annotation.tailrec
import scala.math.Fractional.Implicits.infixFractionalOps

case class Day3(input: String) extends Problem {
  override def solve1(): Unit = {
    @tailrec
    def helper(currentAddress: Complex, instructions: String, acc: Set[Complex]): Set[Complex] = {
      if (instructions.isEmpty) acc
      else {
        val instruction = instructions.head
        val delta = addressDelta(instruction)
        val nextAddress = currentAddress + delta

        helper(nextAddress, instructions.tail, acc + nextAddress)
      }
    }

    val result =  helper(Complex.ZERO, input, Set(Complex.ZERO)).size

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    @tailrec
    def helper(santaAddress: Complex, robotAddress: Complex, instructions: String, acc: Set[Complex]): Set[Complex] = {
      if (instructions.isEmpty) acc
      else {
        val santaInstruction = instructions.head
        val robotInstruction = instructions.tail.head
        val santaDelta = addressDelta(santaInstruction)
        val robotDelta = addressDelta(robotInstruction)
        val nextSantaAddress = santaAddress + santaDelta
        val nextRobotAddress = robotAddress + robotDelta

        helper(nextSantaAddress, nextRobotAddress, instructions.tail.tail, acc + nextSantaAddress + nextRobotAddress)
      }
    }

    val result =  helper(Complex.ZERO, Complex.ZERO, input, Set(Complex.ZERO)).size

    println(s"Result 2: $result")
  }

  private def addressDelta(instruction: Char) = instruction match {
    case '^' => Complex.I
    case '>' => Complex.ONE
    case 'v' => -Complex.I
    case '<' => -Complex.ONE
  }
}

case object Day3 extends App {
  val input = Files.lines("2015/day3.txt").mkString("")
  val problem = Day3(input)
  problem.solve1()
  problem.solve2()
}
