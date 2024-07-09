package space.scown.adventofcode
package advent2016.problems

import lib.{Files, Problem}

case class Day6(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val result = solve(v => v.maxBy(_._2))

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val result = solve(v => v.minBy(_._2))

    println(s"Result 2: $result")
  }

  private def solve(getValue: Vector[(Char, Int)] => (Char, Int)) = {
    val transpose = input.map(_.toVector).transpose
    val result = transpose.map { col =>
      getValue(col.groupBy(identity)
        .view.mapValues(_.size)
        .toVector)
        ._1
    }.mkString("")
    result
  }
}

case object Day6 extends App {
  val input = Files.lines("2016/day6.txt")
  val problem = Day6(input)
  problem.solve1()
  problem.solve2()
}
