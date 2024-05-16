package space.scown.adventofcode
package advent2023.problems

import lib.{Files, Problem}

import scala.annotation.tailrec

case class Day13(lines: Vector[String]) extends Problem {

  override def solve1(): Unit = {
    val grids = findGrids(lines)

    val result = grids.map { gridLines =>
      val grid = Files.grid(gridLines, "")
      val columnGrid = grid.transpose

      @tailrec
      def findReflectedRows(grid: Vector[Vector[Char]], currentRow: Int): Int = {
        if (currentRow == grid.size) -1
        else {
          val pairs = for {
            i <- currentRow until grid.size
            j = i - ((2 * (i - currentRow)) + 1)
            if j >= 0
          } yield (i, j)

          if (pairs.forall(p => grid(p._1) == grid(p._2))) currentRow
          else findReflectedRows(grid, currentRow + 1)
        }
      }

      findReflectedRows(grid, 1) match {
        case -1 => findReflectedRows(columnGrid, 1)
        case x => 100 * x
      }
    }.sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val grids = findGrids(lines)

    val result = grids.map { gridLines =>
      val grid = Files.grid(gridLines, "")
      val columnGrid = grid.transpose

      @tailrec
      def findReflectedRows(grid: Vector[Vector[Char]], currentRow: Int): Int = {
        if (currentRow == grid.size) -1
        else {
          val pairs = for {
            i <- currentRow until grid.size
            j = i - ((2 * (i - currentRow)) + 1)
            if j >= 0
          } yield (i, j)

          val differenceCount = pairs.map {
            case (i, j) => countDifferences(grid(i), grid(j))
          }.sum

          if (differenceCount == 1) currentRow
          else findReflectedRows(grid, currentRow + 1)
        }
      }

      findReflectedRows(grid, 1) match {
        case -1 => findReflectedRows(columnGrid, 1)
        case x => 100 * x
      }
    }.sum

    println(s"Result 2: $result")
  }

  private def findGrids(lines: Vector[String]) = {
    @tailrec
    def helper(remainingLines: Vector[String], acc: Vector[Vector[String]]): Vector[Vector[String]] = {
      if (remainingLines.isEmpty) acc
      else {
        val (next, rest) = remainingLines.span(_.nonEmpty)

        if (rest.isEmpty) acc.appended(next)
        else helper(rest.tail, acc.appended(next))
      }
    }

    helper(lines, Vector())
  }

  private def countDifferences(x: Vector[Char], y: Vector[Char]): Int = {
    x.zip(y).count {
      case (c1, c2) => c1 != c2
    }
  }
}

object Day13 {
  def main(args: Array[String]): Unit = {
    val value = Files.lines("2023/day13.txt")
    Day13(value).solve1()
    Day13(value).solve2()
  }

}
