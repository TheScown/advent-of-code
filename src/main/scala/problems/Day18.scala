package space.scown.advent2023
package problems

import lib.Timer.time
import lib.{Files, Problem}

case class Day18(lines: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val pattern = "([A-Z]) (\\d+)".r
    val instructions = lines.map { line =>
      val matches = pattern.findAllIn(line)

      Instruction(matches.group(1), matches.group(2).toInt)
    }

    val (_, doubleArea) = instructions.foldLeft(((0, 0), 0)) { (acc, instruction) => acc match {
      case (p1, sum) =>
        val p2 = instruction + p1

        (p2, sum + det(p1, p2))
    }}

    val positiveDoubleArea = Math.abs(doubleArea)
    val perimeter = instructions.map(_.distance).sum
    val interiorPoints = (positiveDoubleArea + 2 - perimeter) / 2

    val result = interiorPoints + perimeter

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val pattern = "[A-Z] \\d+ \\(#([0-9a-f]{5})([0-9a-f])\\)".r
    val instructions = lines.map { line =>
      val matches = pattern.findAllIn(line)

      BigInstruction(matches.group(2), BigInt(matches.group(1), 16))
    }

    val (_, doubleArea) = instructions.foldLeft(((BigInt(0), BigInt(0)), BigInt(0))) { (acc, instruction) => acc match {
      case (p1, sum) =>
        val p2 = instruction + p1

        (p2, sum + det(p1, p2))
    }}

    val positiveDoubleArea = doubleArea.abs
    val perimeter = instructions.map(_.distance).sum
    val interiorPoints = (positiveDoubleArea + 2 - perimeter) / 2

    val result = interiorPoints + perimeter

    println(s"Result 2: $result")
  }

  private def det(p1: (Int, Int), p2: (Int, Int)): Int = {
    val (x1, y1) = p1
    val (x2, y2) = p2

    (x1 * y2) - (y1 * x2)
  }

  private def det(p1: (BigInt, BigInt), p2: (BigInt, BigInt)): BigInt = {
    val (x1, y1) = p1
    val (x2, y2) = p2

    (x1 * y2) - (y1 * x2)
  }
}

case class Instruction(direction: String, distance: Int) {

  def +(start: (Int, Int)): (Int, Int) = {
    val (x, y) = start

    direction match {
      case "U" => (x, y + distance)
      case "R" => (x + distance, y)
      case "D" => (x, y - distance)
      case "L" => (x - distance, y)
      case _ => throw new IllegalStateException(s"Invalid direction $direction")
    }
  }

}

case class BigInstruction(direction: String, distance: BigInt) {

  def +(start: (BigInt, BigInt)): (BigInt, BigInt) = {
    val (x, y) = start

    direction match {
      case "0" => (x + distance, y)
      case "1" => (x, y - distance)
      case "2" => (x - distance, y)
      case "3" => (x, y + distance)
      case _ => throw new IllegalStateException(s"Invalid direction $direction")
    }
  }

}

object Day18 {
  def main(args: Array[String]): Unit = {
    val value = Files.lines("day18.txt")
    time(() => Day18(value).solve1())
    time(() => Day18(value).solve2())
  }

}
