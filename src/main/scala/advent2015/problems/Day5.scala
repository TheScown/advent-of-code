package space.scown.adventofcode
package advent2015.problems

import lib.{Files, Problem}

case class Day5(input: Vector[String]) extends Problem {

  override def solve1(): Unit = {
    val result = input.count { s =>
      val vowels = "[aeiou]".r.findAllIn(s).size >= 3
      val repeatedLetter = "([a-z])\\1".r.findFirstMatchIn(s).isDefined
      val naughty = "ab|cd|pq|xy".r.findFirstMatchIn(s).isDefined

      vowels &&
        repeatedLetter &&
        !naughty
    }

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val result = input.count { s =>
      val repeatedPair = "([a-z][a-z])[a-z]*\\1".r.findFirstMatchIn(s).isDefined
      val letterBetween = "([a-z])[a-z]\\1".r.findFirstMatchIn(s).isDefined

      repeatedPair && letterBetween
    }

    println(s"Result 2: $result")
  }
}

case object Day5 extends App {
  val input = Files.lines("2015/day5.txt")
  val problem = Day5(input)
  problem.solve1()
  problem.solve2()
}
