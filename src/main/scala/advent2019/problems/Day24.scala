package space.scown.adventofcode
package advent2019.problems

import lib.{Complex, Files, Grid, Problem}

import scala.annotation.tailrec

case class Day24(input: Vector[String]) extends Problem {
  private type BugGrid = Grid[(Char, Complex[Int])]

  override def solve1(): Unit = {
    val initialGrid = Grid(input.map(_.toVector))

    @tailrec
    def helper(grid: BugGrid, seen: Set[Int]): Int = {
      val biodiversity = grid.indices.zipWithIndex.map { case (address, powerOf2) =>
        grid(address)._1 match {
          case '#' => Math.pow(2, powerOf2).toInt
          case '.' => 0
        }
      }.sum

      if (seen.contains(biodiversity)) biodiversity
      else {
        val newGrid = grid.map { case (value, index) =>
          val neighbours = grid.neighbours(index).map(i => grid(i)._1)

          val newValue = updateCell(value, neighbours)

          (newValue, index)
        }

        helper(newGrid, seen + biodiversity)
      }
    }

    val result = helper(initialGrid.zipWithIndex, Set())

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val initialGrid = Grid(input.map(_.toVector))
    val centre = initialGrid.centre
    val emptyGrid = Grid.of(5, 5, '.').zipWithIndex

    def down(
      grid: BugGrid,
      below: List[BugGrid]
    ): List[BugGrid] = {
      val affectsBelow = Seq(
        grid(centre - Complex.ONE),
        grid(centre + Complex.ONE[Int]),
        grid(centre - Complex.I),
        grid(centre + Complex.I[Int]),
      ).count(_._1 == '#') > 0

      if (below.isEmpty && !affectsBelow) below
      else {
        val nextGrid = if (below.nonEmpty) below.head else emptyGrid
        val belowTail = if (below.nonEmpty) below.tail else Nil
        val updatedNextGrid = updateGrid(nextGrid, List(grid), belowTail)
        updatedNextGrid :: down(nextGrid, belowTail)
      }
    }

    def up(
      grid: BugGrid,
      above: List[BugGrid]
    ): List[BugGrid] = {
      val affectsAbove = grid.filter { case (_, Complex(re, im)) =>
        re == 0 || im == 0 || re == grid.rowLength - 1 || im == -(grid.columnLength - 1)
      }.count(_._1 == '#') > 0

      if (above.isEmpty && !affectsAbove) above
      else {
        val nextGrid = if (above.nonEmpty) above.head else emptyGrid
        val aboveTail = if (above.nonEmpty) above.tail else Nil
        val updatedNextGrid = updateGrid(nextGrid, aboveTail, List(grid))
        updatedNextGrid :: up(nextGrid, aboveTail)
      }
    }

    @tailrec
    def helper(
      grid: BugGrid,
      above: List[BugGrid],
      below: List[BugGrid],
      remainingIterations: Int
    ): (BugGrid, List[BugGrid], List[BugGrid]) = {
      if (remainingIterations == 0) (grid, above, below)
      else {
        // Pull this out, we need it for up and down
        val newGrid = updateGrid(grid, above, below)

        // Call down, which does the above and goes down
        val updatedBelow = down(grid, below)
        // Call up, which does the above and goes up
        val updatedAbove = up(grid, above)

        // Combine the new below and above lists, decrement remaining iterations, and call helper
        helper(newGrid, updatedAbove, updatedBelow, remainingIterations - 1)
      }
    }

    val (finalGrid, above, below) = helper(initialGrid.zipWithIndex, Nil, Nil, 200)

    val result = finalGrid.count(_._1 == '#') +
      above.map(_.count(_._1 == '#')).sum +
      below.map(_.count(_._1 == '#')).sum

    println(s"Result 2: $result")
  }

  private def updateGrid(grid: BugGrid, above: List[BugGrid], below: List[BugGrid]): BugGrid = {
    val centre = grid.centre

    grid.map { case (value, index) =>
      if (index == centre) (value, index)
      else {
        val sameGridNeighbours = grid.neighbours(index).filter(_ != centre).map(i => grid(i)._1)

        val belowNeighbours = {
          if (below.isEmpty) Seq()
          else if (index == centre - Complex.ONE) below.head.filter(_._2.re == 0)
          else if (index == centre + Complex.ONE[Int]) below.head.filter(_._2.re == grid.rowLength - 1)
          else if (index == centre + Complex.I[Int]) below.head.filter(_._2.im == 0)
          else if (index == centre - Complex.I) below.head.filter(_._2.im == -(grid.columnLength - 1))
          else Seq()
        }.map(_._1)

        val aboveNeighbours = {
          if (above.isEmpty) Seq()
          else {
            Seq(
              if (index.re == 0) Seq(above.head(centre - Complex.ONE)) else Seq(),
              if (index.re == grid.rowLength - 1) Seq(above.head(centre + Complex.ONE[Int])) else Seq(),
              if (index.im == 0) Seq(above.head(centre + Complex.I[Int])) else Seq(),
              if (index.im == -(grid.columnLength - 1)) Seq(above.head(centre - Complex.I[Int])) else Seq(),
            ).flatten
          }
        }.map(_._1)

        val neighbours = sameGridNeighbours ++ belowNeighbours ++ aboveNeighbours

        val newValue = updateCell(value, neighbours)

        (newValue, index)
      }
    }
  }

  private def updateCell(value: Char, neighbours: IndexedSeq[Char]) = {
    value match {
      case '#' if neighbours.count(_ == '#') == 1 => '#'
      case '#' => '.'
      case '.' if neighbours.count(_ == '#') == 1 || neighbours.count(_ == '#') == 2 => '#'
      case '.' => '.'
    }
  }
}

case object Day24 extends App {
  val input = Files.lines("2019/day24.txt")
  val problem = Day24(input)
  problem.solve1()
  problem.solve2()
}
