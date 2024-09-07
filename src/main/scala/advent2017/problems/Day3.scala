package space.scown.adventofcode
package advent2017.problems

import lib.{Complex, Files, Problem}

import scala.annotation.tailrec
import scala.math.Numeric.IntIsIntegral

case class Day3(input: String) extends Problem {
  override def solve1(): Unit = {
    val target = input.toInt

    @tailrec
    def helper(current: Complex[Int], remaining: Int, distance: Int, direction: Complex[Int]): Complex[Int] = {
      if (remaining <= distance) {
        current + (direction * remaining)
      }
      else {
        val next = current + direction * distance
        val nextDirection = direction * Complex.I
        val nextDistance = if (direction.isImaginary) distance + 1 else distance

        helper(next, remaining - distance, nextDistance, nextDirection)
      }
    }

    val targetAddress = helper(Complex.ZERO, target - 1, 1, Complex.ONE)
    val result = targetAddress.re.abs + targetAddress.im.abs

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val target = input.toInt

    @tailrec
    def helper(current: Complex[Int], stepsToTurn: Int, initialStepsToTurn: Int, direction: Complex[Int], values: Map[Complex[Int], Int]): Int = {
      val neighbourValues = for {
        i <- -1 to 1
        j <- -1 to 1
        if !(i == 0 && j == 0)
      } yield {
        values.getOrElse(current + Complex(i, j), 0)
      }

      val newValue = neighbourValues.sum

      if (newValue > target) newValue
      else if (stepsToTurn == 0) {
        val nextStepsToTurn = if (direction.isImaginary) initialStepsToTurn + 1 else initialStepsToTurn
        val newDirection = direction * Complex.I
        helper(current + newDirection, nextStepsToTurn - 1, nextStepsToTurn, newDirection, values + (current -> newValue))
      }
      else {
        helper(current + direction, stepsToTurn - 1, initialStepsToTurn, direction, values + (current -> newValue))
      }
    }

    val result = helper(Complex.ONE, 0, 1, Complex.ONE, Map(Complex.ZERO -> 1))

    println(s"Result 2: $result")
  }
}

case object Day3 extends App {
  val input = Files.lines("2017/day3.txt").head
  val problem = Day3(input)
  problem.solve1()
  problem.solve2()
}
