package space.scown.advent2023
package problems

import lib.Timer.time
import lib.{Files, Problem}

import scala.annotation.tailrec

case class Day14(lines: Vector[String]) extends Problem {
  type Grid = Vector[Vector[Char]]

  override def solve1(): Unit = {
    val grid = Files.grid(lines, "")

    val gridAfterTilt = tiltUp(grid)

    val result = loadLevel(gridAfterTilt)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val grid = Files.grid(lines, "")

    def cycle(grid: Grid) = tiltRight(tiltDown(tiltLeft(tiltUp(grid))))

    @tailrec
    def helper(grid: Grid, history: Vector[Grid]): (Int, Vector[Grid]) = {
      val cycled = cycle(grid)
      val historyIndex = history.indexOf(cycled)

      if (historyIndex == -1) helper(cycled, history :+ cycled)
      else (historyIndex, history)
    }

    val (cycleOffset, history) = helper(grid, Vector(grid))
    val cycleLength = history.size - cycleOffset
    val gridBillion = history((1_000_000_000 - cycleOffset) % cycleLength + cycleOffset)
    val result = loadLevel(gridBillion)

    println(s"Result 2: $result")
  }

  private def loadLevel(gridAfterTilt: Grid) = {
    gridAfterTilt.zip(gridAfterTilt.size to 1 by -1).map {
      case (row, index) => row.count(_ == 'O') * index
    }.sum
  }

  @tailrec
  private def tiltUp(remainingRows: Vector[(Vector[Char], Int)], grid: Grid, objects: Vector[Int]): Grid  = {
    if (remainingRows.isEmpty) grid
    else {
      val (row, rowIndex) = remainingRows.head

      @tailrec
      def rowHelper(remainingRow: Vector[(Char, Int)], grid: Grid, objects: Vector[Int]): (Grid, Vector[Int]) = {
        if (remainingRow.isEmpty) (grid, objects)
        else {
          val (next, col) = remainingRow.head

          next match {
            case '.' => rowHelper(remainingRow.tail, grid, objects)
            case '#' => rowHelper(remainingRow.tail, grid, objects.updated(col, rowIndex))
            case 'O' =>
              val destination = objects(col) + 1
              val updatedGrid = grid
                .updated(rowIndex, grid(rowIndex).updated(col, '.'))
                .updated(destination, grid(destination).updated(col, 'O'))
              rowHelper(remainingRow.tail, updatedGrid, objects.updated(col, destination))
          }

        }
      }

      val (updatedGrid, updatedObjects) = rowHelper(row.zipWithIndex, grid, objects)
      tiltUp(remainingRows.tail, updatedGrid, updatedObjects)
    }
  }

  private def tiltUp(grid: Grid): Grid = {
    tiltUp(grid.zipWithIndex, grid, Vector.fill(grid(0).size)(-1))
  }

  private def tiltLeft(grid: Grid): Grid = {
    tiltUp(grid.transpose).transpose
  }

  private def tiltDown(grid: Grid): Grid = {
    tiltUp(grid.reverse).reverse
  }

  private def tiltRight(grid: Grid): Grid = {
    tiltLeft(grid.map(row => row.reverse)).map(row => row.reverse)
  }
}

object Day14 {
  def main(args: Array[String]): Unit = {
    val value = Files.lines("day14.txt")
    Day14(value).solve1()
    time(() => Day14(value).solve2())
  }

}
