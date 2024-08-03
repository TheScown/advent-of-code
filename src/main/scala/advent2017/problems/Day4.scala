package space.scown.adventofcode
package advent2017.problems

import lib.{Files, Problem}

case class Day4(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val result = input.count { line =>
      val words = line.split("\\s")
      words.length == words.toSet.size
    }

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val result = input.count { line =>
      val words = line.split("\\s").toVector
      !words.combinations(2).exists {
        case Vector(a, b) => a.length == b.length && a.toSet == b.toSet
      }
    }

    println(s"Result 2: $result")
  }
}

case object Day4 extends App {
  val input = Files.lines("2017/day4.txt")
  val problem = Day4(input)
  problem.solve1()
  problem.solve2()
}
