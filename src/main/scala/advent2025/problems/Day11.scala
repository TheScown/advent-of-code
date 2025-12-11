package space.scown.adventofcode
package advent2025.problems

import lib.{Files, Problem}

case class Day11(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val graph = parse()

    def helper(currentState: String): Int = {
      if (currentState == "out") 1
      else {
        val nextStates = graph(currentState)

        nextStates.map(helper).sum
      }
    }

    val result = helper("you")

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val graph = parse()

    def helper(currentState: String, visitedDac: Boolean, visitedFft: Boolean, cache: Map[(String, Boolean, Boolean), Long]): (Long, Map[(String, Boolean, Boolean), Long]) = {
      if (currentState == "out")
        (if (visitedDac && visitedFft) 1 else 0, cache)
      else if (cache.contains((currentState, visitedDac, visitedFft))) {
        (cache((currentState, visitedDac, visitedFft)), cache)
      } else {
        val nextStates = graph(currentState)
        val hasVisitedDac = visitedDac || currentState == "dac"
        val hasVisitedFft = visitedFft || currentState == "fft"

        val (result, updatedCache) = nextStates.foldLeft((0L, cache)) { case ((count, cache), next) =>
          val (newCount, updatedCache) =  helper(next, hasVisitedDac, hasVisitedFft, cache)
          (count + newCount, updatedCache)
        }

        (result, updatedCache + ((currentState, hasVisitedDac, hasVisitedFft) -> result))
      }
    }

    val (result, _) = helper("svr", visitedDac = false, visitedFft = false, Map())

    println(s"Result 2: $result")
  }

  private def parse(): Map[String, Vector[String]] = {
    input.map { line =>
      val (lhs, rhs) = line.span(_ != ':')

      lhs -> rhs.tail.trim.split(" ").toVector
    }.toMap
  }
}

case object Day11 extends App {
  val input = Files.lines("2025/day11.txt")
  val problem = Day11(input)
  problem.solve1()
  problem.solve2()
}
