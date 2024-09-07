package space.scown.adventofcode
package advent2018.problems

import lib.{Complex, Files, Grid, Problem}

import scala.math.Numeric.IntIsIntegral

case class Day6(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val points = parse()
    val maxRe = points.maxBy(_.re).re
    val minIm = points.minBy(_.im).im
    val grid = Grid.of(-minIm + 1, maxRe + 1, 0)
    val finalGrid = grid.zipWithIndex.map { case (_, address) =>
      val pairs = points.map(p => (p mh address, p))
      val distances = pairs.sorted { (x: (Int, Complex[Int]), y: (Int, Complex[Int])) =>
        x._1.compareTo(y._1)
      }

      if (distances.head._1 == distances.tail.head._1) None
      else Some(distances.head._2)
    }

    val result = finalGrid.zipWithIndex
      .filter(_._1.isDefined)
      .map(p => (p._1.get, p._2))
      .groupBy(p => p._1)
      .filterNot {
        case (_, matchingPoints) =>
          matchingPoints.exists(p => p._2.re == 0 || p._2.re == maxRe || p._2.im == 0 || p._2.im == minIm)
      }
      .maxBy(_._2.size)._2.size

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val points = parse()
    val maxRe = points.maxBy(_.re).re
    val minIm = points.minBy(_.im).im
    val grid = Grid.of(-minIm + 1, maxRe + 1, 0)
    val finalGrid = grid.zipWithIndex.map { case (_, address) =>
      points.map(p => p mh address).sum
    }
    val result = finalGrid.count(_ < 10000)

    println(s"Result 2: $result")
  }

  private def parse(): Vector[Complex[Int]] = {
    input.map { line =>
      val parts = line.split(", ")
      Complex(parts(0).toInt, -parts(1).toInt)
    }
  }
}

case object Day6 extends App {
  val input = Files.lines("2018/day6.txt")
  val problem = Day6(input)
  problem.solve1()
  problem.solve2()
}