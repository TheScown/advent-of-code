package space.scown.adventofcode
package advent2020

import lib.{Files, Integers, Problem}

import scala.annotation.tailrec

case class Day9(input: Vector[String]) extends Problem {

  override def solve1(): Unit = {
    val values = input.map(_.toLong)
    val valuesToTest = values.zipWithIndex.drop(25)

    val result = findFirstInvalidValue(values, valuesToTest)._1

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val values = input.map(_.toLong)
    val valuesToTest = values.zipWithIndex.drop(25)

    val firstInvalidValue = findFirstInvalidValue(values, valuesToTest)._1

    @tailrec
    def helper(length: Int): Vector[Long] = {
      values.sliding(length).find(window => window.sum == firstInvalidValue) match {
        case Some(window) => window
        case None => helper(length + 1)
      }
    }

    val contiguousRange = helper(2)
    val result = contiguousRange.min + contiguousRange.max

    println(s"Result 2: $result")
  }

  private def findFirstInvalidValue(values: Vector[Long], valuesToTest: Vector[(Long, Int)]) = {
    valuesToTest.find { case (value, index) =>
      val previousValues = values.slice(index - 25, index)
      val pairs = previousValues.combinations(2).toVector
      !pairs.exists {
        case Vector(a, b) =>
          a != b && a + b == value
      }
    }.get
  }

}

case object Day9 extends App {
  val input = Files.lines("2020/day9.txt")
  val problem = Day9(input)
  problem.solve1()
  problem.solve2()
}
