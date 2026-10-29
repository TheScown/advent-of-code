package space.scown.adventofcode
package advent2021

import lib.{Complex, Files, Grid, Problem}

import scala.annotation.tailrec

case class Day20(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val (rules, initialGrid) = parse()

    val finalGrid = enhance(rules, initialGrid, 2, 0)

    val result = finalGrid.count(_ == 1)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val (rules, initialGrid) = parse()

    val finalGrid = enhance(rules, initialGrid, 50, 0)

    val result = finalGrid.count(_ == 1)

    println(s"Result 2: $result")
  }

  @tailrec
  private def enhance(rules: Vector[Int], input: Grid[Int], iterations: Int, defaultValue: Int): Grid[Int] = {
    if (iterations == 0) input
    else {
      val biggerGrid = surroundGrid(input, defaultValue)

      val enhancedGrid = biggerGrid.zipWithIndex.map { case (value, address) =>
        val neighbouringAddresses = neighbours(address)

        val ruleParts = neighbouringAddresses.map(a => biggerGrid.getOrElse(a, defaultValue))

        val rule = ruleParts.zipWithIndex.map {
          case (bit, index) => bit << (9 - index - 1)
        }.reduce(_ | _)

        rules(rule)
      }

      enhance(rules, enhancedGrid, iterations - 1, if (defaultValue == 0) rules.head else rules.last)
    }
  }

  private def parse(): (Vector[Int], Grid[Int]) = {
    val ruleString = input.head
    val rules = ruleString.toVector.map(char2Int)

    val gridLines = input.tail.tail

    val charGrid = Grid(gridLines.map(_.toVector))
    val intGrid = charGrid.map(char2Int)

    (rules, intGrid)
  }

  private def surroundGrid(grid: Grid[Int], defaultValue: Int): Grid[Int] = {
    val biggerGrid = Grid.of(grid.columnLength + 2, grid.rowLength + 2, defaultValue)
    biggerGrid.updated(Complex(1, -1), grid)
  }

  // Need to know neighbours outside the grid, so the grid methods don't work
  private def neighbours(input: Complex[Int]): IndexedSeq[Complex[Int]] = {
    for {
      dy <- 1 to -1 by -1
      dx <- -1 to 1
    } yield Complex(input.re + dx, input.im + dy)
  }

  private def char2Int(c: Char): Int = {
    if (c == '#') 1 else 0
  }
}

case object Day20 extends App {
  val input = Files.lines("2021/day20.txt")
  val problem = Day20(input)
  problem.solve1()
  problem.solve2()
}
