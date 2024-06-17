package space.scown.adventofcode
package advent2015.problems

import lib.{Files, Problem, Timer}

import scala.annotation.tailrec

case class Day25(input: String) extends Problem {
  override def solve1(): Unit = {
    val pattern = "\\d+".r
    val values = pattern.findAllIn(input).map(_.toInt).toVector
    val targetRow = values.head
    val targetColumn = values.tail.head

    @tailrec
    def helper(row: Int, column: Int, acc: Long): Long = {
      if (row == targetRow && column == targetColumn) acc
      else {
        val nextValue = (acc * 252533) % 33554393

        if (row == 1) helper(column + 1, 1, nextValue)
        else helper(row - 1, column + 1, nextValue)
      }
    }

    val result = helper(1, 1, 20151125)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    println("Result 2: Start the weather machine!")
  }
}

case object Day25 extends App {
  val input = Files.lines("2015/day25.txt").head
  val problem = Day25(input)
  Timer.time(() => problem.solve1())
  problem.solve2()
}
