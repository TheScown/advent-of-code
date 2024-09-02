package space.scown.adventofcode
package advent2018.problems

import lib.{Files, Grid, Problem, Timer}

case class Day11(input: String) extends Problem {
  override def solve1(): Unit = {
    val serialNumber = input.toInt

    val grid = fuelCellGrid(serialNumber)

    val resultAddress = grid.indices.filter(c => c.re <= 297 && -c.im <= 297).maxBy { c =>
      val subGrid = grid.slice(c, 3, 3)
      subGrid.values.flatten.sum
    }

    val result = s"${resultAddress.re + 1},${-resultAddress.im + 1}"

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val serialNumber = input.toInt

    val grid = fuelCellGrid(serialNumber)

    val subGridSizes = 1 to 300
    val combinedIndices = grid.indices.iterator.flatMap(c => subGridSizes.map(s => (c, s)))

    val (resultAddress, resultSize) = combinedIndices.filter { case (c, s) =>
      c.re <= 300 - s && -c.im <= 300 - s
    }.maxBy { case (c, s) =>
      val subGrid = grid.slice(c, s, s)
      val sum = subGrid.values.flatten.sum
      sum
    }

    val result = s"${resultAddress.re + 1},${-resultAddress.im + 1},$resultSize"

    println(s"Result 2: $result")
  }

  private def fuelCellGrid(serialNumber: Int): Grid[Int] = {
    Grid.of(300, 300, 0).zipWithIndex.map { case (_, address) =>
      val x = address.re + 1
      val y = -address.im + 1
      val rackId = 10 + x
      val powerLevel = ((y * rackId) + serialNumber) * rackId
      val powerLevelString = powerLevel.toString(10)
      val hundreds = powerLevelString.charAt(powerLevelString.length - 3).asDigit
      hundreds - 5
    }
  }
}

case object Day11 extends App {
  val input = Files.lines("2018/day11.txt").head
  val problem = Day11(input)
  problem.solve1()
  Timer.time(() => problem.solve2())
}
