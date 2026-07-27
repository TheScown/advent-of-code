package space.scown.adventofcode
package advent2021

import lib.{Complex, DFS, Files, Grid, Problem}

case class Day9(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val grid = Grid(input.map(line => line.toVector.map(_.asDigit)))

    val result = findBasinCentres(grid).map(_._1 + 1).sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val grid = Grid(input.map(line => line.toVector.map(_.asDigit)))

    val basinCentres = findBasinCentres(grid).map(_._2)

    val basinSizes = basinCentres.map { address =>
      DFS.reachable(address) { case (address, _) =>
        val currentValue = grid(address)
        val neighbours = grid.neighbours(address)

        neighbours.filter { n =>
          val value = grid(n)
          value > currentValue && value < 9
        }
      }.size
    }

    val result = basinSizes.sorted.takeRight(3).product

    println(s"Result 2: $result")
  }

  private def findBasinCentres(grid: Grid[Int]): Seq[(Int, Complex[Int])] = {
    grid.zipWithIndex.filter { case (value, address) =>
      val neighbours = grid.neighbours(address)

      neighbours.forall(n => grid(n) > value)
    }
  }
}

case object Day9 extends App {
  val input = Files.lines("2021/day9.txt")
  val problem = Day9(input)
  problem.solve1()
  problem.solve2()
}
