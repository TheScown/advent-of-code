package space.scown.adventofcode
package advent2025.problems

import lib.{Files, Grid, Problem}

import scala.annotation.tailrec

case class Day4(input: Vector[String]) extends Problem {

  override def solve1(): Unit = {
    val grid = Grid(input.map(_.toVector))

    val result = grid.zipWithIndex.filter(_._1 == '@').map(_._2).count { c =>
      val neighbours = grid.neighboursWithDiagonals(c)

      neighbours.map(grid.apply).count(_ == '@') < 4
    }

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val grid = Grid(input.map(_.toVector))

    @tailrec
    def helper(currentGrid: Grid[Char], count: Int): Int = {
      val removable = currentGrid.zipWithIndex.filter(_._1 == '@').map(_._2).filter { c =>
        val neighbours = currentGrid.neighboursWithDiagonals(c)

        neighbours.map(currentGrid.apply).count(_ == '@') < 4
      }

      if (removable.isEmpty) count
      else {
        val updatedGrid = removable.foldLeft(currentGrid) { (grid, c) =>
          grid.updated(c, '.')
        }

        helper(updatedGrid, count + removable.size)
      }
    }

    val result = helper(grid, 0)

    println(s"Result 2: $result")
  }

}

case object Day4 extends App {
  val input = Files.lines("2025/day4.txt")
  val problem = Day4(input)
  problem.solve1()
  problem.solve2()
}
