package space.scown.adventofcode
package advent2024.problems

import lib.{Files, Grid, Problem}

import scala.annotation.tailrec

case class Day25(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val (locks, keys) = parse()

    val keyHeights = keys.map(heights)
    val lockHeights = locks.map(heights)

    val result = keyHeights.map { key =>
      lockHeights.count { lock =>
        key.zip(lock).forall { case (k, l) => k + l < 6}
      }
    }.sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {

  }

  private def parse(): (Vector[Grid[Char]], Vector[Grid[Char]]) = {
    @tailrec
    def helper(remainingInput: Vector[String], acc: Vector[Grid[Char]]): Vector[Grid[Char]] = {
      if (remainingInput.isEmpty) acc
      else {
        val (grid, rest) = remainingInput.span(_.nonEmpty)
        val updatedAcc = acc :+ Grid(grid.map(_.toVector))

        if (rest.nonEmpty) helper(rest.tail, updatedAcc)
        else updatedAcc
      }
    }

    val grids = helper(input, Vector())

    grids.partition(_.values.head.forall(_ == '#'))
  }

  private def heights(grid: Grid[Char]): Vector[Int] = {
    grid.values.transpose.map(_.count(_ == '#') - 1)
  }
}

case object Day25 extends App {
  val input = Files.lines("2024/day25.txt")
  val problem = Day25(input)
  problem.solve1()
  problem.solve2()
}
