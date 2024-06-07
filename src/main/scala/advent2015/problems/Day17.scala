package space.scown.adventofcode
package advent2015.problems

import lib.{Files, Problem}

case class Day17(input: Vector[String]) extends Problem {
  private val containers = input.map(_.toInt)

  override def solve1(): Unit = {
    def helper(remainingContainers: Vector[Int], remainingAmount: Int): Int = {
      if (remainingAmount == 0) 1
      else if (remainingContainers.isEmpty) 0
      else {
        val nextContainer = remainingContainers.head
        val withContainer = if (nextContainer <= remainingAmount) {
          helper(remainingContainers.tail, remainingAmount - nextContainer)
        } else 0
        val withoutContainer = helper(remainingContainers.tail, remainingAmount)

        withContainer + withoutContainer
      }
    }

    val result = helper(containers, 150)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    def helper(remainingContainers: Vector[Int], remainingAmount: Int, acc: Vector[Int]): Vector[Int] = {
      if (remainingAmount == 0)
        Vector(acc.size)
      else if (remainingContainers.isEmpty) Vector()
      else {
        val nextContainer = remainingContainers.head
        val withContainer = if (nextContainer <= remainingAmount) {
          helper(remainingContainers.tail, remainingAmount - nextContainer, acc :+ nextContainer)
        } else Vector()
        val withoutContainer = helper(remainingContainers.tail, remainingAmount, acc)

        withContainer ++ withoutContainer
      }
    }

    val combinations = helper(containers, 150, Vector())
    val minSize = combinations.min
    val result = combinations.count(c => c == minSize)

    println(s"Result 2: $result")
  }
}

case object Day17 extends App {
  val input = Files.lines("2015/day17.txt")
  val problem = Day17(input)
  problem.solve1()
  problem.solve2()
}
