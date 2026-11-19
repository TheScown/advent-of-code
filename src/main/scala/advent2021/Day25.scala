package space.scown.adventofcode
package advent2021

import lib.{Complex, Files, Grid, Problem}

import scala.annotation.tailrec

case class Day25(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val grid = Grid(input.map(_.toVector), wrapping = true)

    @tailrec
    def helper(grid: Grid[Char], step: Int): Int = {
      val indexed = grid.zipWithIndex
      val rights = indexed.filter(_._1 == '>')
      val downs = indexed.filter(_._1 == 'v')

      val afterRights = rights.foldLeft(grid) { case (acc, (_, address)) =>
        val next = grid.next(address, Complex.ONE)
        val nextSpace = grid.apply(next)

        if (nextSpace == '.') acc.updated(address, '.').updated(next, '>')
        else acc
      }

      val afterDowns = downs.foldLeft(afterRights) { case (acc, (_, address)) =>
        val next = afterRights.next(address, -Complex.I[Int])
        val nextSpace = afterRights.apply(next)

        if (nextSpace == '.') acc.updated(address, '.').updated(next, 'v')
        else acc
      }

      if (afterDowns == grid) step
      else helper(afterDowns, step + 1)
    }

    val result = helper(grid, 1)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    println("Boost the signal!")
  }
}

case object Day25 extends App {
  val input = Files.lines("2021/day25.txt")
  val problem = Day25(input)
  problem.solve1()
  problem.solve2()
}
