package space.scown.adventofcode
package advent2018.problems

import lib.{Files, Problem}

import scala.annotation.tailrec

case class Day12(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val (initialState, instructionMap) = parse()

    @tailrec
    def helper(state: Map[Int, Boolean], remainingGenerations: Int): Int = {
      if (remainingGenerations == 0) {
        state.filter(_._2).keys.sum
      }
      else {
        val next = germinate(state, instructionMap)

        helper(next, remainingGenerations - 1)
      }
    }

    val result = helper(initialState, 20)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val (initialState, instructionMap) = parse()

    @tailrec
    def helper(state: Map[Int, Boolean], previousState: Map[Int, Boolean], previousDiff: Int, iterations: Int): Long = {
      val diff = state.filter(_._2).keys.sum - previousState.filter(_._2).keys.sum
      if (diff == previousDiff) {
        (50_000_000_000L - iterations) * diff + state.filter(_._2).keys.sum
      }
      else {
        val next = germinate(state, instructionMap)

        helper(next, state, diff, iterations + 1)
      }
    }

    val result = helper(initialState, Map(), 0, 0)

    println(s"Result 2: $result")
  }

  private def germinate(state: Map[Int, Boolean], instructionMap: Map[Vector[Boolean], Boolean]): Map[Int, Boolean] = {
    val (minI, maxI) = (state.keys.min - 2, state.keys.max + 2)

    (minI to maxI).foldLeft(Map[Int, Boolean]()) { (acc, i) =>
      val toTest = (i - 2 to i + 2).map(j => state.getOrElse(j, false)).toVector

      acc + (i -> instructionMap.getOrElse(toTest, false))
    }
  }

  private def parse(): (Map[Int, Boolean], Map[Vector[Boolean], Boolean]) = {
    val (initialPots, rest) = input.splitAt(1)
    val instructions = rest.tail

    val initialState = initialPots.head.replaceFirst(".*?: ", "").zipWithIndex.map { case (c, i) => (i, c == '#')}.toMap
    val instructionMap = instructions.map { line =>
      val parts = line.split(" => ")
      (parts(0).toVector.map(_ == '#'), parts(1) == "#")
    }.toMap

    (initialState, instructionMap)
  }
}

case object Day12 extends App {
  val input = Files.lines("2018/day12.txt")
  val problem = Day12(input)
  problem.solve1()
  problem.solve2()
}
