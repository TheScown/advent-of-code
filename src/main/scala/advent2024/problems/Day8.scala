package space.scown.adventofcode
package advent2024.problems

import lib.{Complex, Files, Grid, Integers, Problem}

case class Day8(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val grid = Grid(input.map(_.toVector))
    val antennae = grid.zipWithIndex.filter(_._1 != '.')
    val groupedAntennae = antennae.groupBy(_._1).values
    val gridIndices = grid.indices.toSet

    val antiNodes = groupedAntennae.flatMap { g =>
      g.map(_._2).combinations(2).flatMap { antennae =>
        val a = antennae.head
        val b = antennae.tail.head
        val delta = a - b

        Seq(
          a + delta,
          b - delta
        ).filter(gridIndices.contains)
      }
    }.toSet

    val result = antiNodes.size

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val grid = Grid(input.map(_.toVector))
    val antennae = grid.zipWithIndex.filter(_._1 != '.')
    val groupedAntennae = antennae.groupBy(_._1).values
    val gridIndices = grid.indices.toSet

    val antiNodes = groupedAntennae.flatMap { g =>
      g.map(_._2).combinations(2).flatMap { antennae =>
        val a = antennae.head
        val b = antennae.tail.head
        val delta = a - b
        val simplified = delta.simplify

        val positives = (0 #:: Integers.naturalNumbers[Int]).map(i => a + Complex(i, 0) * simplified).takeWhile(gridIndices.contains)
        val negatives = Integers.naturalNumbers[Int].map(-_).map(i => a + Complex(i, 0) * simplified).takeWhile(gridIndices.contains)

        positives ++ negatives
      }
    }.toSet

    val result = antiNodes.size

    println(s"Result 2: $result")
  }
}

case object Day8 extends App {
  val input = Files.lines("2024/day8.txt")
  val problem = Day8(input)
  problem.solve1()
  problem.solve2()
}
