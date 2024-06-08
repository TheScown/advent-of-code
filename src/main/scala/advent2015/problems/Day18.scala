package space.scown.adventofcode
package advent2015.problems

import lib.{Complex, Files, Grid, Problem}

case class Day18(input: Vector[Vector[Char]]) extends Problem {
  private val grid = Grid(input)

  override def solve1(): Unit = {
    val result = (1 to 100).foldLeft(grid.zipWithIndex) { (g, _) =>
      g.map {
        case (value, address) =>
          val neighbours = g.neighboursWithDiagonals(address)
          val neighbourValues = neighbours.map(c => g(c))
          val on = neighbourValues.filter(p => p._1 == '#')
          value match {
            case '#' => (if (on.size == 2 || on.size == 3) '#' else '.', address)
            case '.' => (if (on.size == 3) '#' else '.', address)
          }
      }
    }.map(p => p._1).count(c => c == '#')

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val brokenAddresses = Set(
      Complex(0, 0),
      Complex(grid.rowLength - 1, 0),
      Complex(0, -(grid.columnLength - 1)),
      Complex(grid.rowLength - 1, -(grid.columnLength - 1))
    )

    val updatedGrid = brokenAddresses.foldLeft(grid) {
      case (g, address) => g.updated(address, '#')
    }

    val result = (1 to 100).foldLeft(updatedGrid.zipWithIndex) { (g, _) =>
      g.map {
        case (value, address) =>
          if (brokenAddresses.contains(address)) ('#', address)
          else {
            val neighbours = g.neighboursWithDiagonals(address)
            val neighbourValues = neighbours.map(c => g(c))
            val on = neighbourValues.filter(p => p._1 == '#')
            value match {
              case '#' => (if (on.size == 2 || on.size == 3) '#' else '.', address)
              case '.' => (if (on.size == 3) '#' else '.', address)
            }
          }
      }
    }.map(p => p._1).count(c => c == '#')

    println(s"Result 2: $result")
  }
}

case object Day18 extends App {
  val input = Files.grid("2015/day18.txt")
  val problem = Day18(input)
  problem.solve1()
  problem.solve2()
}
