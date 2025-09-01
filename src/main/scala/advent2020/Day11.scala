package space.scown.adventofcode
package advent2020

import lib.{Complex, Files, Grid, Problem}

import scala.annotation.tailrec

case class Day11(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val grid = Grid(input.map(_.toVector))

    @tailrec
    def helper(currentGrid: Grid[Char], previousGrid: Grid[Char], count: Int): Grid[Char] = {
      if (count > 0 && currentGrid == previousGrid) currentGrid
      else {
        val newGrid = currentGrid.zipWithIndex.map { case (value, address) =>
          val neighbours = currentGrid.neighboursWithDiagonals(address)
          val neighbourValues = neighbours.map(currentGrid.apply)
          value match {
            case '.' => '.'
            case 'L' => if (neighbourValues.count(_ == '#') == 0) '#' else 'L'
            case '#' => if (neighbourValues.count(_ == '#') >= 4) 'L' else '#'
          }
        }

        helper(newGrid, currentGrid, count + 1)
      }
    }

    val finalGrid = helper(grid, grid, 0)
    val result = finalGrid.count(_ == '#')

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val grid = Grid(input.map(_.toVector))

    @tailrec
    def helper(currentGrid: Grid[Char], previousGrid: Grid[Char], count: Int): Grid[Char] = {
      if (count > 0 && currentGrid == previousGrid) currentGrid
      else {
        val newGrid = currentGrid.zipWithIndex.map { case (value, address) =>
          // Use line of site to first chair in each direction
          val neighbours = currentGrid.neighboursWithDiagonals(address)
          val validDirections = neighbours.map(_ - address)

          val visibleOccupiedChairs = validDirections.count { direction =>
            @tailrec
            def helper(position: Complex[Int]): Boolean = {
              val newPosition = currentGrid.next(position, direction)

              if (newPosition == position) false
              else currentGrid(newPosition) match {
                case '#' => true
                case 'L' => false
                case '.' => helper(newPosition)
              }
            }

            helper(address)
          }

          value match {
            case '.' => '.'
            case 'L' => if (visibleOccupiedChairs == 0) '#' else 'L'
            case '#' => if (visibleOccupiedChairs >= 5) 'L' else '#'
          }
        }

        helper(newGrid, currentGrid, count + 1)
      }
    }

    val finalGrid = helper(grid, grid, 0)
    val result = finalGrid.count(_ == '#')
    // > 127
    println(s"Result 2: $result")
  }
}

case object Day11 extends App {
  val input = Files.lines("2020/day11.txt")
  val problem = Day11(input)
  problem.solve1()
  problem.solve2()
}
