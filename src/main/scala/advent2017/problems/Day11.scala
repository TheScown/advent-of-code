package space.scown.adventofcode
package advent2017.problems

import lib.{Complex, Files, Problem}

import scala.annotation.tailrec
import scala.math.Integral.Implicits.infixIntegralOps

case class Day11(input: String) extends Problem {
  override def solve1(): Unit = {
    // Use Complex to stand in for Eisenstein integers
    val commands = input.split(",").map {
      case "n" => Complex.ONE
      case "s" => -Complex.ONE
      case "sw" => Complex.I
      case "ne" => -Complex.I
      case "nw" => Complex.ONE + Complex.I
      case "se" => (-Complex.ONE) - Complex.I
    }

    val finalDestination = commands.sum
    val result = eisenhattanDistance(finalDestination)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val commands = input.split(",").map {
      case "n" => Complex.ONE
      case "s" => -Complex.ONE
      case "sw" => Complex.I
      case "ne" => -Complex.I
      case "nw" => Complex.ONE + Complex.I
      case "se" => (-Complex.ONE) - Complex.I
    }

    @tailrec
    def helper(commands: List[Complex], current: Complex, max: BigInt): BigInt = {
      if (commands.isEmpty) max
      else {
        val next = current + commands.head
        val distance = eisenhattanDistance(next)

        if (distance > max) helper(commands.tail, next, distance)
        else helper(commands.tail, next, max)
      }
    }

    val result = helper(commands.toList, Complex.ZERO, 0)

    println(s"Result 2: $result")
  }

  private def eisenhattanDistance(value: Complex): BigInt = {
    val reAbs = value.re.abs
    val imAbs = value.im.abs

    if (value.re.signum != value.im.signum) reAbs + imAbs
    else if (reAbs >= imAbs) reAbs else imAbs
  }
}

case object Day11 extends App {
  val input = Files.lines("2017/day11.txt").head
  val problem = Day11(input)
  problem.solve1()
  problem.solve2()
}