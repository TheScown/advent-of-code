package space.scown.adventofcode
package advent2017.problems

import advent2017.knothash.KnotHash
import advent2017.knothash.KnotHash.HashState
import lib.{Files, Problem}

case class Day10(input: String) extends Problem {
  override def solve1(): Unit = {
    val lengths = if (input.isEmpty) Nil else input.split(",").map(_.toInt).toList

    val finalList = KnotHash.knotHashRound(HashState((0 to 255).toVector, 0, 0, lengths)).values
    val result = finalList(0) * finalList(1)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val result = KnotHash.knotHash(input)

    println(s"Result 2: $result")
  }
}

case object Day10 extends App {
  val input = Files.lines("2017/day10.txt").head
  val problem = Day10(input)
  problem.solve1()
  problem.solve2()
}
