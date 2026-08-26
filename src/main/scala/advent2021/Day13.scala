package space.scown.adventofcode
package advent2021

import lib.{Complex, Files, Grid, Gui, Problem}

import java.awt.Color
import java.awt.image.BufferedImage

case class Day13(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val (dots, folds) = parse()

    val grid = buildGrid(dots, folds)

    val finalGrid = foldGrid(grid, folds.head)
    val result = finalGrid.count(v => v)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val (dots, folds) = parse()

    val grid = buildGrid(dots, folds)

    val finalGrid = folds.foldLeft(grid)(foldGrid)

    val image = finalGrid.zipWithIndex.foldLeft(new BufferedImage(finalGrid.rowLength, finalGrid.columnLength, BufferedImage.TYPE_INT_RGB)) { (image, item) =>
      val (value, index) = item
      val colour = if (value) Color.BLACK else Color.WHITE
      image.setRGB(index.re, -index.im, colour.getRGB)

      image
    }

    Gui.renderImage(image)

    println("Result 2: See window")
  }

  private def foldGrid(grid: Grid[Boolean], fold: (String, Int)): Grid[Boolean] = fold match {
    case (direction, value) =>
      val (grid1, grid2) = direction match {
        case "x" =>
          val grid1 = grid.slice(Complex.ZERO, value, grid.columnLength)
          val grid2 = grid.slice(Complex(value + 1, 0), value, grid.columnLength).flipHorizontally()

          (grid1, grid2)
        case "y" =>
          val grid1 = grid.slice(Complex.ZERO, grid.rowLength, value)
          val grid2 = grid.slice(Complex(0, -(value + 1)), grid.rowLength, value).flipVertically()

          (grid1, grid2)
      }

      grid1.zipWithIndex.map { case (b, address) =>
        b || grid2(address)
      }
  }

  private def buildGrid(dots: Vector[Complex[Int]], folds: Vector[(String, Int)]): Grid[Boolean] = {
    val firstX = folds.find(_._1 == "x").get
    val firstY = folds.find(_._1 == "y").get

    val rows = firstY._2 * 2 + 1
    val columns = firstX._2 * 2 + 1

    dots.foldLeft(Grid.of(rows, columns, false)) { (grid, dotAddress) =>
      grid.updated(dotAddress, true)
    }
  }

  private def parse(): (Vector[Complex[Int]], Vector[(String, Int)]) = {
    val (dotLines, foldLines) = input.span(_.nonEmpty)

    val dots = dotLines.map { line =>
      val ns = line.split(",").map(_.toInt)
      Complex(ns.head, -ns(1))
    }

    val foldPattern = "fold along ([xy])=(\\d+)".r

    val folds = foldLines.tail.map {
      case foldPattern(direction, value) => (direction, value.toInt)
    }

    (dots, folds)
  }
}

case object Day13 extends App {
  val input = Files.lines("2021/day13.txt")
  val problem = Day13(input)
  problem.solve1()
  problem.solve2()
}
