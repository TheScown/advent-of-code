package space.scown.adventofcode
package advent2017.problems

import lib.{Complex, Files, Grid, Problem, Timer}

import scala.annotation.tailrec
import scala.math.Integral.Implicits.infixIntegralOps

case class Day22(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val initialGrid = Grid(input.map(_.toVector))
    val infiniteGrid = initialGrid.toMap
    val initialPosition = initialGrid.centre
    val initialDirection = Complex.I

    val numberOfIterations = 10000

    @tailrec
    def helper(position: Complex, direction: Complex, grid: Map[Complex, Char], acc: Int, iterationCount: Int): Int = {
      if (iterationCount == numberOfIterations) acc
      else {
        grid.getOrElse(position, '.') match {
          case '.' =>
            val newDirection = direction * Complex.I
            helper(position + newDirection, newDirection, grid + (position -> '#'), acc + 1, iterationCount + 1)
          case '#' =>
            val newDirection = direction * -Complex.I
            helper(position + newDirection, newDirection, grid + (position -> '.'), acc, iterationCount + 1)
        }
      }
    }

    val result = helper(initialPosition, initialDirection, infiniteGrid, 0, 0)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val initialGrid = Grid(input.map(_.toVector)).map {
      case '#' => Infected
      case '.' => Clean
    }

    val infiniteGrid = initialGrid.toMap
    val initialPosition = initialGrid.centre
    val initialDirection = Complex.I

    val numberOfIterations = 10000000

    @tailrec
    def helper(position: Complex, direction: Complex, grid: Map[Complex, State], acc: Int, iterationCount: Int): Int = {
      if (iterationCount == numberOfIterations) acc
      else {
        grid.getOrElse(position, Clean) match {
          case Clean =>
            val newDirection = direction * Complex.I
            helper(position + newDirection, newDirection, grid + (position -> Weakened), acc, iterationCount + 1)
          case Weakened =>
            helper(position + direction, direction, grid + (position -> Infected), acc + 1, iterationCount + 1)
          case Infected =>
            val newDirection = direction * -Complex.I
            helper(position + newDirection, newDirection, grid + (position -> Flagged), acc, iterationCount + 1)
          case Flagged =>
            val newDirection = -direction
            helper(position + newDirection, newDirection, grid + (position -> Clean), acc, iterationCount + 1)
        }
      }
    }

    val result = helper(initialPosition, initialDirection, infiniteGrid, 0, 0)

    println(s"Result 2: $result")
  }

  sealed trait State
  private case object Clean extends State
  private case object Weakened extends State
  private case object Infected extends State
  private case object Flagged extends State
}

case object Day22 extends App {
  val input = Files.lines("2017/day22.txt")
  val problem = Day22(input)
  problem.solve1()
  Timer.time(() => problem.solve2())
}
