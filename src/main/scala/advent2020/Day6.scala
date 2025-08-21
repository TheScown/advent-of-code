package space.scown.adventofcode
package advent2020

import lib.{Files, Problem}

case class Day6(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val groups = input.mkString("\n").split("\n\n").toVector
    val result = groups.map { groupString =>
      val groupAsSets = groupString.trim().split("\n").toVector.map(_.toSet)

      val union = groupAsSets.reduce(_ union _)

      union.size
    }.sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val groups = input.mkString("\n").split("\n\n").toVector
    val result = groups.map { groupString =>
      val groupAsSets = groupString.trim().split("\n").toVector.map(_.toSet)

      val intersection = groupAsSets.reduce(_ intersect _)

      intersection.size
    }.sum

    println(s"Result 1: $result")
  }
}

case object Day6 extends App {
  val input = Files.lines("2020/day6.txt")
  val problem = Day6(input)
  problem.solve1()
  problem.solve2()
}

