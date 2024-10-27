package space.scown.adventofcode
package advent2018.problems

import lib.{Files, Problem, UndirectedGraph, Vec3}

import scala.annotation.tailrec

case class Day23(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val nanobots = parse()
    val largestRange = nanobots.maxBy(_.range)
    val result = nanobots.count { nanobot =>
      largestRange.inRange(nanobot)
    }

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val nanobots = parse()
    val nanobotClusters = nanobots.map(nanobot => nanobots.filter(other => other.overlapsWith(nanobot)))
    val largestCluster = nanobotClusters.maxBy(_.size)
    val ranges = largestCluster.sortBy(-_.range).map(_.manhattanRange)

    val resultRange = ranges.reduce { (acc, underTest) =>
      if (underTest.end < acc.start || underTest.start > acc.end) acc
      else if (acc.start <= underTest.start && acc.end >= underTest.end) underTest
      else if (acc.start <= underTest.start && acc.end <= underTest.end) NanobotRange(underTest.start, acc.end)
      else NanobotRange(acc.start, underTest.end)
    }
    val result = resultRange.start

    println(s"Result 2: $result")
  }

  private def parse(): Vector[Nanobot] = {
    val pattern = "pos=<(-?\\d+),(-?\\d+),(-?\\d+)>, r=(\\d+)".r

    input.zipWithIndex.map {
      case (pattern(x, y, z, r), index) => Nanobot(index, Vec3(x.toLong, y.toLong, z.toLong), r.toLong)
    }
  }

  private def buildGraph(nanobots: Vector[Nanobot]): UndirectedGraph[Nanobot] = {
    @tailrec
    def helper(remainingNanobots: Vector[Nanobot], acc: UndirectedGraph[Nanobot]): UndirectedGraph[Nanobot] = {
      if (remainingNanobots.isEmpty) acc
      else {
        val next = remainingNanobots.head
        val rest = remainingNanobots.tail
        val overlapsWithHead = rest.filter(other => next.overlapsWith(other))
        val edges = overlapsWithHead.flatMap(other => Seq(UndirectedGraph.Edge(next, other, 0)))
        helper(rest, (acc + next).addEdges(edges))
      }
    }

    helper(nanobots, UndirectedGraph())
  }

  private case class NanobotRange(start: Long, end: Long) {
    val size: Long = end - start
  }

  private case class Nanobot(id: Int, position: Vec3[Long], range: Long) {
    def inRange(other: Nanobot): Boolean = {
      (position - other.position).norm1 <= range
    }

    def manhattanRange: NanobotRange = {
      val norm = position.norm1
      NanobotRange(norm - range, norm + range)
    }

    def overlapsWith(other: Nanobot): Boolean = {
      val myRange = manhattanRange
      val otherRange = other.manhattanRange

      if (myRange.start <= otherRange.start) myRange.end >= otherRange.start
      else otherRange.end >= myRange.start
    }
  }
}

case object Day23 extends App {
  val input = Files.lines("2018/day23.txt")
  val problem = Day23(input)
  problem.solve1()
  problem.solve2()
}
