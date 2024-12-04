package space.scown.adventofcode
package advent2024

import lib.{Complex, Files, Grid, Problem}

case class Day4(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val grid = Grid(input.map(_.toVector))

    val directions = Seq[Complex[Int]](
      Complex.ONE,
      Complex.I,
      -Complex.ONE[Int],
      -Complex.I[Int],
      Complex(1, 1),
      Complex(-1, 1),
      Complex(-1, -1),
      Complex(1, -1)
    )

    val indicies = grid.indices.toSet

    val result = grid.zipWithIndex.filter(_._1 == 'X').map { case (_, index) =>
      val ranges = directions.map { d =>
        (0 until 4).map(index + d * _)
      }

      val validRanges = ranges.filter { range =>
        indicies.contains(range.last)
      }

      validRanges.map(_.map(grid.apply).mkString("")).count(_ == "XMAS")
    }.sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val grid = Grid(input.map(_.toVector))

    val directions = Seq(
      Complex(1, 1),
      Complex(-1, 1),
      Complex(-1, -1),
      Complex(1, -1)
    )

    val indicies = grid.indices.toSet

    val result = grid.zipWithIndex.filter(_._1 == 'A').count { case (_, index) =>
      val ranges = directions.map { d =>
        (-1 to 1).map(index + d * _)
      }

      val validRanges = ranges.filter { range =>
        val start = range.head
        val end = range.last

        indicies.contains(start) && indicies.contains(end)
      }

      validRanges.map(_.map(grid.apply).mkString("")).count(_ == "MAS") == 2
    }

    println(s"Result 2: $result")
  }
}

case object Day4 extends App {
  val input = Files.lines("2024/day4.txt")
  val problem = Day4(input)
  problem.solve1()
  problem.solve2()
}
