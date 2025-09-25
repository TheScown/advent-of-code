package space.scown.adventofcode
package advent2020

import lib.{Complex, Files, Grid, Problem}

case class Day17(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val grid = Grid(input.map(_.toVector))

    def emptyGrid(size: Int): Grid[Char] = Grid.of(size, size, '.')

    def updateGrid(grid: Grid[Char], above: Grid[Char], below: Grid[Char]): Grid[Char] = {
      grid.zipWithIndex.map { case (c, index) =>
        val localNeighbourAddresses = grid.neighboursWithDiagonals(index)
        val neighbourAddresses = index +: localNeighbourAddresses

        val localNeighbours = localNeighbourAddresses.map(grid.apply)
        val aboveNeighbours = neighbourAddresses.map(above.apply)
        val belowNeighbours = neighbourAddresses.map(below.apply)

        val activeNeighbours = localNeighbours.count(isActive) + aboveNeighbours.count(isActive) + belowNeighbours.count(isActive)

        if (isActive(c)) {
          if (activeNeighbours == 2 || activeNeighbours == 3) '#' else '.'
        }
        else {
          if (activeNeighbours == 3) '#' else '.'
        }
      }
    }

    def expandGrid(grid: Grid[Char]): Grid[Char] = {
      val newSize = grid.rowLength + 2
      val newGrid = emptyGrid(newSize)

      newGrid.updated(Complex(1, -1), grid)
    }

    def hasActiveEdge(grid: Grid[Char]): Boolean = {
      val edgeAddresses = grid.indices.filter { address =>
        (address.re == 0 || address.re == grid.rowLength - 1) || (address.im == 0 || address.im == -(grid.columnLength - 1))
      }
      edgeAddresses.exists(address => isActive(grid(address)))
    }

    def travel(grid: Grid[Char], otherGrids: List[Grid[Char]]): List[Grid[Char]] = {
      val affectsOtherGrid = grid.exists(isActive)

      if (otherGrids.isEmpty && !affectsOtherGrid) otherGrids
      else {
        val nextGrid = if (otherGrids.isEmpty) emptyGrid(grid.rowLength) else otherGrids.head
        val tail = if (otherGrids.isEmpty) Nil else otherGrids.tail
        val nextNextGrid = if (tail.isEmpty) emptyGrid(grid.rowLength) else tail.head

        val updatedGrid = updateGrid(nextGrid, grid, nextNextGrid)

        updatedGrid :: travel(nextGrid, tail)
      }
    }

    println(grid)

    val (finalGrid, finalAbove, finalBelow) = (0 until  6).foldLeft((grid, List[Grid[Char]](), List[Grid[Char]]())) { case ((grid, above, below), i) =>
      println(i)

      val needsExpansion = hasActiveEdge(grid)

      val (expandedGrid, expandedAbove, expandedBelow) = if (!needsExpansion) (grid, above, below) else {
        val expandedGrid = expandGrid(grid)
        val expandedAbove = above.map(expandGrid)
        val expandedBelow = below.map(expandGrid)

        (expandedGrid, expandedAbove, expandedBelow)
      }

      val aboveGrid = if (expandedAbove.nonEmpty) expandedAbove.head else emptyGrid(expandedGrid.rowLength)
      val belowGrid = if (expandedBelow.nonEmpty) expandedBelow.head else emptyGrid(expandedGrid.rowLength)

      val updatedGrid = updateGrid(expandedGrid, aboveGrid, belowGrid)
      val updatedAbove = travel(expandedGrid, expandedAbove)
      val updatedBelow = travel(expandedGrid, expandedBelow)

      println(updatedAbove)
      println(updatedGrid)
      println(updatedBelow)

      (updatedGrid, updatedAbove, updatedBelow)
    }

    val result = finalGrid.count(isActive) +
      finalAbove.map(_.count(isActive)).sum +
      finalBelow.map(_.count(isActive)).sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {

  }

  private def isActive(c: Char): Boolean = c == '#'
}

case object Day17 extends App {
  val input = Files.lines("2020/day17.txt")
  val problem = Day17(input)
  problem.solve1()
  problem.solve2()
}
