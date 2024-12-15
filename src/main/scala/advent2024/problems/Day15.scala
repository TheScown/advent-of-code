package space.scown.adventofcode
package advent2024.problems

import lib.{Complex, Files, Grid, Problem}

case class Day15(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val (grid, instructions, robotLocation) = parse()

    def move(grid: Grid[Char], location: Complex[Int], direction: Complex[Int]): (Grid[Char], Complex[Int]) = {
      val next = grid.next(location, direction)
      grid(next) match {
        case '#' => (grid, location)
        case '.' => (grid.updated(next, grid(location)).updated(location, '.'), next)
        case 'O' =>
          val (newGrid, newLocation) = move(grid, next, direction)

          if (newLocation == next) (newGrid, location)
          else (newGrid.updated(next, newGrid(location)).updated(location, '.'), next)
      }
    }

    val (finalGrid, _) = instructions.foldLeft((grid, robotLocation)) { case ((grid, robotLocation), instruction) =>
      move(grid, robotLocation, instruction)
    }

    val result = finalGrid.zipWithIndex.filter(_._1 == 'O').map(p => p._2.re - 100 * p._2.im).sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val (grid, instructions, _) = parse()
    val expandedGrid = Grid(grid.values.map { row =>
      row.flatMap {
        case '.' => Vector('.', '.')
        case '#' => Vector('#', '#')
        case 'O' => Vector('[', ']')
        case '@' => Vector('@', '.')
      }
    })
    val expandedRobotLocation = expandedGrid.indexOf('@').get

    def move(grid: Grid[Char], location: Complex[Int], direction: Complex[Int]): (Grid[Char], Complex[Int]) = {
      val next = grid.next(location, direction)

      def moveBoxHorizontally = {
        val (newGrid, newLocation) = move(grid, next, direction)

        if (newLocation == next) (grid, location)
        else (newGrid.updated(next, newGrid(location)).updated(location, '.'), next)
      }

      def moveBoxVertically(otherHalfNext: Complex[Int]) = {
        val (newGrid, newLocation) = move(grid, next, direction)

        if (newLocation == next) (grid, location)
        else {
          val (otherHalfGrid, otherHalfNewLocation) = move(newGrid, otherHalfNext, direction)

          if (otherHalfNewLocation == otherHalfNext) (grid, location)
          else {
            val finalGrid = otherHalfGrid
              .updated(next, otherHalfGrid(location))
              .updated(location, '.')
              .updated(otherHalfNext, otherHalfGrid(otherHalfNext))
              .updated(otherHalfNext, '.')

            (finalGrid, next)
          }
        }
      }

      grid(next) match {
        case '#' => (grid, location)
        case '.' => (grid.updated(next, grid(location)).updated(location, '.'), next)
        case '[' =>
          if (direction.isReal) moveBoxHorizontally
          else {
            val otherHalfNext = next + Complex.ONE[Int]
            moveBoxVertically(otherHalfNext)
          }
        case ']' =>
          if (direction.isReal) moveBoxHorizontally
          else {
            val otherHalfNext = next - Complex.ONE[Int]
            moveBoxVertically(otherHalfNext)
          }
      }
    }

    val (finalGrid, _) = instructions.foldLeft((expandedGrid, expandedRobotLocation)) { case ((grid, robotLocation), instruction) =>
      move(grid, robotLocation, instruction)
    }

    val result = finalGrid.zipWithIndex.filter(_._1 == '[').map(p => p._2.re - 100 * p._2.im).sum

    println(s"Result 2: $result")
  }

  private def parse(): (Grid[Char], Vector[Complex[Int]], Complex[Int]) = {
    val (gridLines, instructionLines) = input.span(_.nonEmpty)

    val grid = Grid(gridLines.map(_.toVector))

    val instructions = instructionLines.tail.flatMap { line =>
      line.map {
        case '^' => Complex.I[Int]
        case 'v' => -Complex.I[Int]
        case '>' => Complex.ONE[Int]
        case '<' => -Complex.ONE[Int]
      }
    }

    val robotLocation = grid.indexOf('@').get

    (grid, instructions, robotLocation)
  }
}

case object Day15 extends App {
  val input = Files.lines("2024/day15.txt")
  val problem = Day15(input)
  problem.solve1()
  problem.solve2()
}
