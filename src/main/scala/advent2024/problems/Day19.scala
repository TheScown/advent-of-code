package space.scown.adventofcode
package advent2024.problems

import lib.{BFS, Files, Problem}

case class Day19(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val (towels, patterns) = parse()

    val result = patterns.count { pattern =>
      val initialState = pattern

      BFS.solve[String](initialState)(_.isEmpty) { (remainingPattern, _) =>
        towels
          .filter(remainingPattern.startsWith)
          .map(towel => remainingPattern.substring(towel.length))
      }.isDefined
    }

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val (towels, patterns) = parse()

    val cache = Map[String, Long]()

    val (_, result) = patterns.foldLeft((cache, 0L)) { case ((cache, total), pattern) =>
      def helper(remainingPattern: String, cache: Map[String, Long]): (Map[String, Long], Long) = {
        if (cache.contains(remainingPattern)) (cache, cache(remainingPattern))
        else if (remainingPattern.isEmpty) (cache, 1L)
        else {
          val usableTowels = towels.filter(remainingPattern.startsWith)

          val (finalCache, sum) = usableTowels.foldLeft((cache, 0L)) { case ((cache, total), towel) =>
            val (updatedCache, sum) = helper(remainingPattern.substring(towel.length), cache)
            (updatedCache + (remainingPattern -> (total + sum)), total + sum)
          }

          (finalCache, sum)
        }
      }

      val (updatedCache, sum) = helper(pattern, cache)
      (updatedCache + (pattern -> (total + sum)), total + sum)
    }

    println(s"Result 2: $result")
  }

  private def parse(): (Vector[String], Vector[String]) = {
    val towels = input.head.split(", ").toVector

    val patterns = input.drop(2)

    (towels, patterns)
  }

  case class State(remainingString: String, tokens: Vector[String])
}

case object Day19 extends App {
  val input = Files.lines("2024/day19.txt")
  val problem = Day19(input)
  problem.solve1()
  problem.solve2()
}
