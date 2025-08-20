package space.scown.adventofcode
package advent2020

import lib.{Files, Problem}

case class Day5(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val result = input.map(lineToSeatId).max

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val seatIds = input.map(lineToSeatId).sorted

    val result = seatIds.zipWithIndex.find { case (id, index) =>
      seatIds(index + 1) != id + 1
    }.get._1 + 1

    println(s"Result 2: $result")
  }

  private def lineToSeatId(line: String): Int = {
    val rowString = line.substring(0, 7).replace("B", "1").replace("F", "0")
    val seatString = line.substring(7).replace("R", "1").replace("L", "0")

    val row = Integer.valueOf(rowString, 2)
    val seat = Integer.valueOf(seatString, 2)

    row * 8 + seat
  }
}

case object Day5 extends App {
  val input = Files.lines("2020/day5.txt")
  val problem = Day5(input)
  problem.solve1()
  problem.solve2()
}
