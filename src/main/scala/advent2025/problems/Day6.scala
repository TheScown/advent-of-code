package space.scown.adventofcode
package advent2025.problems

import lib.{Files, Grid, Problem}

import scala.annotation.tailrec

case class Day6(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val gridItems = input.map(_.split("\\s+").toVector)

    val transposed = gridItems.transpose

    val result = transposed.map { row =>
      val (values, operation) = row.splitAt(row.size - 1)
      val numericValues = values.map(_.toLong)

      operation.head match {
        case "+" => numericValues.sum
        case "*" => numericValues.product
      }
    }.sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val paddedLines = Grid.padLines(input)

    val gridItems = paddedLines.map(_.toVector)
    val transposed = gridItems.transpose.reverse

    @tailrec
    def helper(remainingRows: Vector[Vector[Char]], acc: Long): Long = {
      val (problemLines, rest) = remainingRows.span(_.mkString("").trim.nonEmpty)
      val (numberLines, lastLines) = problemLines.splitAt(problemLines.size - 1)
      val lastLine = lastLines.head
      val (lastNumber, operator) = lastLine.splitAt(lastLine.size - 1)

      val values = (numberLines :+ lastNumber).map(_.mkString("").trim.toLong)

      val result = operator.head match {
        case '+' => values.sum
        case '*' => values.product
      }

      val newAcc = acc + result

      if (rest.isEmpty) newAcc
      else helper(rest.tail, newAcc)
    }

    val result = helper(transposed, 0)

    println(s"Result 2: $result")
  }

}

case object Day6 extends App {
  val input = Files.lines("2025/day6.txt")
  val problem = Day6(input)
  problem.solve1()
  problem.solve2()
}
