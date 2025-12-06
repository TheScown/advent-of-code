package space.scown.adventofcode
package advent2017.problems

import lib.{Complex, Files, Grid, Problem}

import scala.annotation.tailrec
import scala.math.Numeric.IntIsIntegral


case class Day19(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val grid = buildGrid()
    val start = grid.indexOf('|').get

    val pathCharacters = Set('-', '|')

    @tailrec
    def helper(position: Complex[Int], direction: Complex[Int], acc: Vector[Char]): Vector[Char] = {
      if (grid(position) == '+') {
        val nextPosition = grid.neighbours(position)
          .filterNot(grid(_) == ' ')
          .filterNot(_ == position - direction)
          .head
        val nextDirection = nextPosition - position

        helper(nextPosition, nextDirection, acc)
      }
      else if (grid(position) == ' ') acc
      else if (pathCharacters.contains(grid(position))) helper(position + direction, direction, acc)
      else helper(position + direction, direction, acc :+ grid(position))
    }

    val result = helper(start, -Complex.I, Vector()).mkString("")

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val grid = buildGrid()
    val start = grid.indexOf('|').get

    @tailrec
    def helper(position: Complex[Int], direction: Complex[Int], acc: Int): Int = {
      if (grid(position) == '+') {
        val nextPosition = grid.neighbours(position)
          .filterNot(grid(_) == ' ')
          .filterNot(_ == position - direction)
          .head
        val nextDirection = nextPosition - position

        helper(nextPosition, nextDirection, acc + 1)
      }
      else if (grid(position) == ' ') acc - 1
      else helper(position + direction, direction, acc + 1)
    }

    val result = helper(start, -Complex.I, 1)

    println(s"Result 2: $result")
  }

  private def buildGrid(): Grid[Char] = {
    val paddedLines = Grid.padLines(input)

    Grid(paddedLines.map(_.toVector))
  }
}

case object Day19 extends App {
  val input = Files.lines("2017/day19.txt")
  val problem = Day19(input)
  problem.solve1()
  problem.solve2()
}
