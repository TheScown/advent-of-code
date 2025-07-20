package space.scown.adventofcode
package advent2019.problems

import lib.{BFS, Complex, Files, Grid, Problem}

case class Day20(input: Vector[String]) extends Problem {

  override def solve1(): Unit = {
    val grid = parse()

    val pointsToLabels = grid
      .zipWithIndex
      .filter(_._1 == '.')
      .foldLeft(Map[Complex[Int], String]()) { case (acc, (_, address)) =>
        if (grid(address + Complex.ONE[Int]).isUpper) acc + (address -> (grid(address + Complex.ONE[Int]).toString + grid(address + Complex.ONE[Int] * 2).toString))
        else if (grid(address - Complex.ONE[Int]).isUpper) acc + (address -> (grid(address - Complex.ONE[Int] * 2).toString + grid(address - Complex.ONE[Int]).toString))
        else if (grid(address + Complex.I[Int]).isUpper) acc + (address -> (grid(address + Complex.I[Int] * 2).toString + grid(address + Complex.I[Int]).toString))
        else if (grid(address - Complex.I[Int]).isUpper) acc + (address -> (grid(address - Complex.I[Int]).toString + grid(address - Complex.I[Int] * 2).toString))
        else acc
      }

    val labelsToPoints = pointsToLabels.groupMap(_._2)(_._1)

    val startAddress = labelsToPoints("AA").head
    val targetAddress = labelsToPoints("ZZ").head

    val result = BFS.solve(startAddress)(_ == targetAddress) { (currentAddress, _) =>
      val neighbours = grid.neighbours(currentAddress).filter(a => grid(a) == '.')

      if (pointsToLabels.contains(currentAddress)) {
        val label = pointsToLabels(currentAddress)
        val destinations = labelsToPoints(label)

        if (destinations.size > 1) {
          if (destinations.head == currentAddress) neighbours :+ destinations.tail.head
          else neighbours :+ destinations.head
        }
        else neighbours
      }
      else neighbours
    }.get.steps

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val grid: Grid[Char] = parse()
    val rowLength = grid.rowLength
    val columnLength = grid.columnLength

    val pointsToLabels = grid
      .zipWithIndex
      .filter(_._1 == '.')
      .foldLeft(Map[Complex[Int], String]()) { case (acc, (_, address)) =>
        if (grid(address + Complex.ONE[Int]).isUpper) acc + (address -> (grid(address + Complex.ONE[Int]).toString + grid(address + Complex.ONE[Int] * 2).toString))
        else if (grid(address - Complex.ONE[Int]).isUpper) acc + (address -> (grid(address - Complex.ONE[Int] * 2).toString + grid(address - Complex.ONE[Int]).toString))
        else if (grid(address + Complex.I[Int]).isUpper) acc + (address -> (grid(address + Complex.I[Int] * 2).toString + grid(address + Complex.I[Int]).toString))
        else if (grid(address - Complex.I[Int]).isUpper) acc + (address -> (grid(address - Complex.I[Int]).toString + grid(address - Complex.I[Int] * 2).toString))
        else acc
      }

    val labelsToPoints = pointsToLabels.groupMap(_._2)(_._1)

    val startAddress = labelsToPoints("AA").head
    val targetAddress = labelsToPoints("ZZ").head

    val startState = State(startAddress, 0)

    val result = BFS.solve(startState)(s => s.depth == 0 && s.address == targetAddress) { case (State(currentAddress, depth), _) =>
      val neighbours = grid.neighbours(currentAddress).filter(a => grid(a) == '.').map(n => State(n, depth))

      if (pointsToLabels.contains(currentAddress)) {
        val label = pointsToLabels(currentAddress)
        val destinations = labelsToPoints(label)

        if (destinations.size > 1) {
          val nextDestination = if (destinations.head == currentAddress) destinations.tail.head
          else destinations.head

          if (
            currentAddress.re == 2 || currentAddress.re == rowLength - 3
            || currentAddress.im == -2 || currentAddress.im == -columnLength + 3
          ) {
            if (depth > 0) {
              neighbours :+ State(nextDestination, depth - 1)
            }
            else neighbours
          }
          else neighbours :+ State(nextDestination, depth + 1)
        }
        else neighbours
      }
      else neighbours
    }.get.steps

    println(s"Result 2: $result")
  }

  private def parse() = {
    val maxLineLength = input.maxBy(_.length).length
    val fixedLines = input.map { line =>
      if (line.length < maxLineLength) {
        val missing = maxLineLength - line.length
        line + " " * missing
      }
      else line
    }

    Grid(fixedLines.map(line => line.toVector))
  }

  case class State(address: Complex[Int], depth: Int)
}

case object Day20 extends App {
  val input = Files.lines("2019/day20.txt")
  val problem = Day20(input)
  problem.solve1()
  problem.solve2()
}
