package space.scown.advent2023
package problems

import lib.{Files, Problem}

case class Day9(lines: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val result = lines.map { line =>
      val values = line.split("\\s+").map(_.toLong).toVector

      def helper(currentValues: Vector[Long]): Long = {
        if (currentValues.forall(x => x == 0)) 0
        else {
          val nextDifference = helper(differences(currentValues))
          currentValues.last + nextDifference
        }
      }

      helper(values)
    }.sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val result = lines.map { line =>
      val values = line.split("\\s+").map(_.toLong).toVector

      def helper(currentValues: Vector[Long]): Long = {
        if (currentValues.forall(x => x == 0)) 0
        else {
          val nextDifference = helper(differences(currentValues))
          currentValues.head - nextDifference
        }
      }

      helper(values)
    }.sum

    println(s"Result 2: $result")
  }

  private def differences(values: Vector[Long]): Vector[Long] = {
    values.zip(values.tail).map {
      case (i, j) => j - i
    }
  }
}

object Day9 {
  def main(args: Array[String]): Unit = {
    val value = Files.lines("day9.txt")
    Day9(value).solve1()
    Day9(value).solve2()
  }

}
