package space.scown.adventofcode
package advent2016

import lib.{Complex, Files, Problem}

import scala.annotation.tailrec
import scala.collection.immutable.NumericRange
import scala.math.Integral.Implicits.infixIntegralOps

case class Day1(input: String) extends Problem {
  override def solve1(): Unit = {
    val instructions = parse()

    val (finalPosition, _) = instructions.foldLeft((Complex.ZERO, Complex.I)) { (acc, instruction) =>
      val (position, direction) = acc
      val newDirection = instruction.direction * direction
      val newPosition = position + (newDirection * instruction.distance)
      (newPosition, newDirection)
    }

    val result = finalPosition.re.abs + finalPosition.im.abs

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val instructions = parse()

    @tailrec
    def helper(position: Complex, direction: Complex, visited: Set[Complex], instructions: Seq[Instruction]): Complex = {
      val nextInstruction = instructions.head
      val newDirection = nextInstruction.direction * direction
      val newPosition = position + (newDirection * nextInstruction.distance)
      val toVisit = Complex.linearRange(position, newPosition, newDirection)

      toVisit.find(c => visited.contains(c)) match {
        case Some(c) => c
        case None => helper(newPosition, newDirection, visited ++ toVisit, instructions.tail)
      }
    }

    val visitedTwice = helper(Complex.ZERO, Complex.I, Set(), instructions)
    val result = visitedTwice.re.abs + visitedTwice.im.abs

    println(s"Result 2: $result")
  }

  private def parse(): Seq[Instruction] = {
    val pattern = "([LR])(\\d+)".r

    input.split(", ").map {
      case pattern(direction, distance) => Instruction(
        direction = direction match {
          case "L" => Complex.I
          case "R" => -Complex.I
        },
        distance = distance.toInt
      )
    }
  }

  case class Instruction(direction: Complex, distance: Int)
}

case object Day1 extends App {
  val input = Files.lines("2016/day1.txt")
  val problem = Day1(input.head)
  problem.solve1()
  problem.solve2()
}
