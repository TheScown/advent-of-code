package space.scown.adventofcode
package advent2019.problems

import advent2019.intcode.{IntcodeComputer, IntcodeProgram, RequiresInput}
import lib.{Files, Integers, Problem}

case class Day19(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val program = IntcodeProgram.fromLines(input)
    val computer = IntcodeComputer(program).execute()

    val results = for {
      x <- 0 until 50
      y <- 0 until 50
    } yield {
      Seq(x, y).foldLeft(computer) { (output, i) =>
        output match {
          case RequiresInput(_, continue) => continue(i)
        }
      }.outputs.head
    }

    val result = results.count(_ == 1)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    // Before this the beam doesn't have a pattern (determined by inspection)
    val beamStartOrigin = (6, 8)
    val beamEndOrigin = (20, 25)
    val desiredSquareLength = 100

    val (x, y) = Integers.naturalNumbers[Int]
      .drop(24)
      .map(y => (y, beamStart(y, beamStartOrigin), beamEnd(y, beamEndOrigin)))
      .map { case (y, startOfBeam, endOfBeam) =>
        val nextBeamStart = beamStart(y + desiredSquareLength - 1, beamStartOrigin)
        val nextBeamEnd = beamEnd(y + desiredSquareLength - 1, beamEndOrigin)

        (startOfBeam until endOfBeam).find { x =>
          if (x + desiredSquareLength > endOfBeam) false
          else {
            x >= nextBeamStart && x < nextBeamEnd && x + desiredSquareLength - 1 >= nextBeamStart && x + desiredSquareLength - 1 < nextBeamEnd
          }
        }.map(x => (x, y))
      }
      .find(_.isDefined)
      .get.get

    val result = 10000 * x + y

    println(s"Result 2: $result")
  }

  /**
   * @param y      The row
   * @param origin The first row point where the pattern holds
   * @return The first x for which the computer returns 1
   */
  private def beamStart(y: Int, origin: (Int, Int)): Int = {
    val delta = y - origin._2
    val quotient = delta / 3
    val remainder = delta % 3

    val possibleErrorChecks = y / 325

    val error = if (possibleErrorChecks == 0) 0 else {
      val errorExists = (0 until  possibleErrorChecks).exists { i =>
        val startOfSequence = 325 + 325 * i

        if (y < startOfSequence) false
        else {
          val remainder = y - startOfSequence
          remainder % 3 == 0
        }
      }
      if (errorExists) 1 else 0
    }

    quotient * 2 + (if (remainder != 0) 1 else 0) + origin._1 + error
  }

  /**
   * @param y      The row
   * @param origin The first beam row where the pattern holds
   * @return The first x after the last x where the computer returns 1
   */
  private def beamEnd(y: Int, origin: (Int, Int)): Int = {
    val delta = y - origin._2
    val quotient = delta / 23
    val remainder = delta % 23

    val additionalSteps = remainder match {
      case r if r >= 22 => r - 4
      case r if r >= 16 => r - 3
      case r if r >= 10 => r - 2
      case r if r >= 4 => r - 1
      case r => r
    }

    val possibleErrorChecks = y / 213

    val error = if (possibleErrorChecks == 0) 0 else {
      val errorExists = (0 to possibleErrorChecks).exists { i =>
        val startOfSequence = 213 + 190 * i

        if (y < startOfSequence) false
        else {
          val remainder = y - startOfSequence
          remainder % 23 == 0
        }
      }
      if (errorExists) 1 else 0
    }

    quotient * 19 + additionalSteps + 1 + origin._1 + error
  }
}

case object Day19 extends App {
  val input = Files.lines("2019/day19.txt")
  val problem = Day19(input)
  problem.solve1()
  problem.solve2()
}
