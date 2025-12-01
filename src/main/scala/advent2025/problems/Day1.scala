package space.scown.adventofcode
package advent2025.problems

import lib.{Files, Problem}

case class Day1(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val instructions = input.map { line =>
      line.toVector match {
        case 'L' +: rest => -rest.mkString("").toInt % 100
        case 'R' +: rest => rest.mkString("").toInt % 100
      }
    }

    val (_, result) = instructions.foldLeft((50, 0)) { case ((position, count), instruction) =>
      val newPosition = position + instruction match {
        case n if n < 0 => n + 100
        case n if n > 99 => n - 100
        case n => n
      }

      val newCount = if (newPosition == 0) count + 1 else count

      (newPosition, newCount)
    }

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val instructions = input.map { line =>
      line.toVector match {
        case 'L' +: rest => -rest.mkString("").toInt
        case 'R' +: rest => rest.mkString("").toInt
      }
    }

    val (_, result) = instructions.foldLeft((50, 0)) { case ((position, count), instruction) =>
      val fullRotationCounts = instruction.abs / 100
      val remainder = instruction % 100

      val newPosition = position + remainder match {
        case n if n < 0 => n + 100
        case n if n > 99 => n - 100
        case n => n
      }

      // If it overflows (and we didn't start at 0), we crossed zero
      val crossedZero = position + remainder match {
        case n if n <= 0 && position > 0 => 1
        case n if n >= 100 && position > 0 => 1
        case _ => 0
      }

      val newCount = count + fullRotationCounts + crossedZero

      (newPosition, newCount)
    }

    println(s"Result 2: $result")
  }
}

case object Day1 extends App {
  val input = Files.lines("2025/day1.txt")
  val problem = Day1(input)
  problem.solve1()
  problem.solve2()
}
