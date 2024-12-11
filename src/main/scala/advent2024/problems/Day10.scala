package space.scown.adventofcode
package advent2024.problems

import lib.{BFS, Files, Grid, Problem}

case class Day10(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val grid = Grid(input.map(_.toVector.map(_.asDigit))).zipWithIndex

    val trailheads = grid.filter(_._1 == 0)

    val result = trailheads.map { pair =>
      BFS.reachable(pair) { case (value, index) =>
        grid.neighbours(index).map(grid.apply).filter(_._1 == value + 1)
      }.count(_.value._1 == 9)
    }.sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val grid = Grid(input.map(_.toVector.map(_.asDigit))).zipWithIndex

    val trailheads = grid.filter(_._1 == 0)

    val result = trailheads.map { pair =>
      BFS.reachable(List(pair)) { list =>
        val (value, index) = list.head
        val nexts = grid.neighbours(index).map(grid.apply).filter(_._1 == value + 1)

        nexts.map(_ :: list)
      }.count(_.value.head._1 == 9)
    }.sum

    println(s"Result 2: $result")
  }
}

case object Day10 extends App {
  val input = Files.lines("2024/day10.txt")
  val problem = Day10(input)
  problem.solve1()
  problem.solve2()
}
