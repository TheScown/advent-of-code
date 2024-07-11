package space.scown.adventofcode
package advent2016.problems

import lib.{Files, Problem}

import scala.annotation.tailrec

case class Day9(input: String) extends Problem {
  override def solve1(): Unit = {
    @tailrec
    def helper(s: StringBuilder, sum: Int): Int = {
      if (s.isEmpty) sum
      else s.head match {
        case '(' =>
          val (instruction, remainder) = s.tail.span(c => c != ')')
          val pattern = "(\\d+)x(\\d+)".r
          instruction match {
            case pattern(count, repetitions) =>
              val decompressedSize = count.toInt * repetitions.toInt
              helper(remainder.tail.drop(count.toInt), sum + decompressedSize)
          }
        case _ => helper(s.tail, sum + 1)
      }
    }

    val result = helper(new StringBuilder(input), 0)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    def helper(s: StringBuilder, sum: Long): Long = {
      if (s.isEmpty) sum
      else s.head match {
        case '(' =>
          val (instruction, remainder) = s.tail.span(c => c != ')')
          val pattern = "(\\d+)x(\\d+)".r
          instruction match {
            case pattern(count, repetitions) =>
              val toExpand = remainder.tail.take(count.toInt)
              val expandedSize = helper(new StringBuilder(toExpand.toString()), 0)
              val decompressedSize = expandedSize * repetitions.toInt
              helper(remainder.tail.drop(count.toInt), sum + decompressedSize)
          }
        case _ => helper(s.tail, sum + 1)
      }
    }

    val result = helper(new StringBuilder(input), 0)

    println(s"Result 2: $result")
  }
}

case object Day9 extends App {
  val input = Files.lines("2016/day9.txt").head
  val problem = Day9(input)
  problem.solve1()
  problem.solve2()
}
