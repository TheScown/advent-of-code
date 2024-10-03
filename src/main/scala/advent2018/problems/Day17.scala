package space.scown.adventofcode
package advent2018.problems

import lib._

import scala.math.Numeric.IntIsIntegral

case class Day17(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val (grid, offset, minY) = parse()

//    val panel = new GridPanel[Element](grid, 1900, 31900)
//    val window = getPanelWindow(panel, 500, 500)
//    window.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)

    val startPosition = Complex(500 - offset, -minY)
    val finalGrid = simulate(startPosition, grid)
    val matching = finalGrid.zipWithIndex.filter(e => e._1 == StillWater || e._1 == FlowingWater)
    val result = matching.size

    // < 38033, > 29655
    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val (grid, offset, minY) = parse()

    val startPosition = Complex(500 - offset, -minY)
    val finalGrid = simulate(startPosition, grid)
    val matching = finalGrid.zipWithIndex.filter(e => e._1 == StillWater)
    val result = matching.size

    println(s"Result 2: $result")
  }

  private def simulate(position: Complex[Int], grid: Grid[Element]): Grid[Element] = {
    def helper(position: Complex[Int], previous: Complex[Int], grid: Grid[Element]): (Grid[Element], Set[Complex[Int]]) = {
      //      panel.setGrid(grid)
      val belowAddress = grid.next(position, -Complex.I)

      if (belowAddress == position) (grid.updated(position, FlowingWater), Set())
      else {
        val below = grid(belowAddress)

        if (below == FlowingWater) (grid.updated(position, FlowingWater), Set())
        else {
          val (gridAfterBelow, _) = if (below == Sand) helper(belowAddress, position, grid.updated(position, FlowingWater)) else (grid, Set())
          val updatedBelow = gridAfterBelow(belowAddress)

          if (updatedBelow == FlowingWater) (gridAfterBelow.updated(position, FlowingWater), Set())
          else {
            val leftAddress = position - Complex.ONE
            val rightAddress = position + Complex.ONE
            val left = gridAfterBelow(leftAddress)
            val right = gridAfterBelow(rightAddress)

            val (afterLeft, possiblyStillLeft) = if (left == Sand) helper(leftAddress, position, gridAfterBelow.updated(position, FlowingWater)) else (gridAfterBelow, Set[Complex[Int]]())
            val (afterRight, possiblyStillRight) = if (right == Sand) helper(rightAddress, position, afterLeft.updated(position, FlowingWater)) else (afterLeft, Set[Complex[Int]]())

            val aboveAddress = position + Complex.I

            val result = if (
              (previous == leftAddress && (right == Clay || possiblyStillRight.contains(rightAddress)))
                || (previous == rightAddress && (left == Clay || possiblyStillLeft.contains(leftAddress)))
            ) {
              (afterRight.updated(position, FlowingWater), (possiblyStillLeft ++ possiblyStillRight) + position)
            }
            else if (previous == aboveAddress && (left == Clay || possiblyStillLeft.contains(leftAddress)) && (right == Clay || possiblyStillRight.contains(rightAddress))) {
              val toUpdate = (possiblyStillLeft ++ possiblyStillRight) + position
              val finalGrid = toUpdate.foldLeft(afterRight) { (grid, address) =>
                grid.updated(address, StillWater)
              }
              (finalGrid, Set[Complex[Int]]())
            }
            else {
              (afterRight.updated(position, FlowingWater), Set[Complex[Int]]())
            }

            //            panel.setGrid(result._1)
            result
          }
        }
      }
    }

    helper(position, position + Complex.I, grid)._1
  }

  private def parse(): (Grid[Element], Int, Int) = {
    val pattern = "([xy])=(\\d+), ([xy])=(\\d+)..(\\d+)".r

    val survey = input.map {
      case pattern(fixed, fixedValue, variable, rangeStart, rangeEnd) => (
        (fixed.head, fixedValue.toInt),
        (variable.head, rangeStart.toInt to rangeEnd.toInt)
      )
    }

    val minX = survey.map {
      case ((fixed, fixedValue), (_, range)) =>
        if (fixed == 'x') fixedValue
        else range.start
    }.min - 1

    val maxX = survey.map {
      case ((fixed, fixedValue), (_, range)) =>
        if (fixed == 'x') fixedValue
        else range.end
    }.max + 1

    val minY = survey.map {
      case ((fixed, fixedValue), (_, range)) =>
        if (fixed == 'y') fixedValue
        else range.start
    }.min

    val maxY = survey.map {
      case ((fixed, fixedValue), (_, range)) =>
        if (fixed == 'y') fixedValue
        else range.end
    }.max

    val grid = survey.foldLeft(Grid.of[Element](maxY + 1, maxX - minX + 1, Sand)) { (grid, instruction) =>
      instruction match {
        case ((fixed, fixedValue), (_, range)) =>
          val indexRange = if (fixed == 'x') Complex.linearRange(Complex(fixedValue - minX, -range.start), Complex(fixedValue - minX, -range.end), -Complex.I, true)
            else Complex.linearRange(Complex(range.start - minX, -fixedValue), Complex(range.end - minX, -fixedValue), Complex.ONE, true)

          indexRange.foldLeft(grid) { (grid, address) =>
            grid.updated(address, Clay)
          }
      }
    }

    (grid, minX, minY)
  }

  trait Element
  case object Sand extends Element {
    override def toString: String = "."
  }
  case object Clay extends Element {
    override def toString: String = "#"
  }
  case object FlowingWater extends Element {
    override def toString: String = "|"
  }
  case object StillWater extends Element {
    override def toString: String = "~"
  }
}

case object Day17 extends App {
  val input = Files.lines("2018/day17.txt")
  val problem = Day17(input)
  problem.solve1()
  problem.solve2()
}
