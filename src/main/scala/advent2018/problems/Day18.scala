package space.scown.adventofcode
package advent2018.problems

import lib.{Files, Grid, Problem, Timer}

import scala.annotation.tailrec

case class Day18(input: Vector[String]) extends Problem {

  override def solve1(): Unit = {
    val grid = Grid(input.map(_.toVector))

    val finalGrid = (0 until 10).foldLeft(grid)((grid, _) => advance(grid))

    val result = value(finalGrid)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val grid = Grid(input.map(_.toVector))

    @tailrec
    def helper(grid: Grid[Char], history: Vector[Grid[Char]], count: Int): (Vector[Grid[Char]], Int) = {
      val gridIndex = history.indexOf(grid)

      if (gridIndex >= 0) (history, gridIndex)
      else helper(advance(grid), history :+ grid, count + 1)
    }

    val (grids, offset) = helper(grid, Vector(), 0)
    val cycle = grids.drop(offset)
    val finalGridIndex = (1000000000L - offset) % cycle.size
    val finalGrid = cycle(finalGridIndex.toInt)

    val result = value(finalGrid)

    println(s"Result 2: $result")
  }

  private def advance(grid: Grid[Char]): Grid[Char] = {
    grid.zipWithIndex.map { case (c, address) =>
      val neighbours = grid.neighboursWithDiagonals(address)
      val neighbourValues = neighbours.map(grid.apply)

      c match {
        case '.' => if (neighbourValues.count(_ == '|') >= 3) '|' else '.'
        case '|' => if (neighbourValues.count(_ == '#') >= 3) '#' else '|'
        case '#' => if (neighbourValues.contains('|') && neighbourValues.contains('#')) '#' else '.'
      }
    }
  }

  private def value(grid: Grid[Char]) = {
    val treeCount = grid.count(_ == '|')
    val lumberyardCount = grid.count(_ == '#')
    val result = treeCount * lumberyardCount
    result
  }

}

case object Day18 extends App {
  val input = Files.lines("2018/day18.txt")
  val problem = Day18(input)
  problem.solve1()
  Timer.time(() => problem.solve2())
}
