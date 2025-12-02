package space.scown.adventofcode
package advent2025.problems

import lib.{Files, Problem}

import scala.collection.immutable.NumericRange

case class Day2(input: Vector[String]) extends Problem {

  override def solve1(): Unit = {
    val ranges = parse()

    val result = ranges.flatMap { _.filter { i =>
      val s = i.toString
      val length = s.length

      if (length % 2 != 0) false
      else {
        val (firstHalf, secondHalf) = (s.substring(0, length / 2), s.substring(length / 2))

        firstHalf == secondHalf
      }
    }}.sum

    println(s"Result 1:$result")
  }

  override def solve2(): Unit = {
    val ranges = parse()

    val result = ranges.flatMap { _.filter { i =>
      val s = i.toString
      val length = s.length

      (1 to length / 2).exists { i =>
        if (length % i != 0) false
        else {
          val repetition = s.substring(0, i)
          repetition.repeat(length / i) == s
        }
      }
    }}.sum

    println(s"Result 2:$result")
  }

  private def parse(): Vector[NumericRange[Long]] = {
    val rangeStrings = input.head.split(",")

    rangeStrings.map { rs =>
      val parts = rs.split("-")
      Range.Long.inclusive(parts(0).toLong, parts(1).toLong, 1)
    }.toVector
  }

}

case object Day2 extends App {
  val input = Files.lines("2025/day2.txt")
  val problem = Day2(input)
  problem.solve1()
  problem.solve2()
}
