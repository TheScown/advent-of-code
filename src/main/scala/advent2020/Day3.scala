package space.scown.adventofcode
package advent2020

import lib.{Complex, Files, Grid, Problem}

import scala.annotation.tailrec

case class Day3(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val grid = Grid(input.map(_.toVector))
    val delta = Complex(3, -1)

    val result = countTrees(grid, delta, Complex.ZERO, 0)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val grid = Grid(input.map(_.toVector))
    val deltas = Seq(
      Complex(1, -1),
      Complex(3, -1),
      Complex(5, -1),
      Complex(7, -1),
      Complex(1, -2),
    )

    val trees = deltas.map(countTrees(grid, _, Complex.ZERO, 0))
    val result = trees.product

    println(s"Result 2: $result")
  }

  @tailrec
  private def countTrees(grid: Grid[Char], slope: Complex[Int], address: Complex[Int], count: Int): Int = {
    if (address.im <= -grid.columnLength) count
    else {
      val newCount = count + (grid(address) match {
        case '#' => 1
        case '.' => 0
      })

      val newAddress = address + slope
      val wrappedNewAddress = Complex(newAddress.re % grid.rowLength, newAddress.im)

      countTrees(grid, slope, wrappedNewAddress, newCount)
    }
  }
}

case object Day3 extends App {
  val input = Files.lines("2020/day3.txt")
  val problem = Day3(input)
  problem.solve1()
  problem.solve2()
}
