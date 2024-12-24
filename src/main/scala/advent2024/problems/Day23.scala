package space.scown.adventofcode
package advent2024.problems

import lib.{Files, Problem, UndirectedGraph, UnionFind}

import java.lang.Long.parseLong

case class Day23(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val graph = parseGraph()

    val triples = graph.nodes.foldLeft(Set[Set[String]]()) { (acc, node) =>
      val neighbours = graph.neighbours(node).toVector
      val neighbourPairs = neighbours.combinations(2)
      val matchingNeighbourPairs = neighbourPairs.filter {
        case Vector(n1, n2) => graph.neighbours(n1).contains(n2)
      }

      acc union matchingNeighbourPairs.map {
        case Vector(n1, n2) => Set(node, n1, n2)
      }.toSet
    }

    val result = triples.count(set => set.exists(_.startsWith("t")))

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val graph = parseGraph()

    val maximalCliques = graph.maximalCliques()
    val lanParty = maximalCliques.maxBy(_.size).toVector
    val result = lanParty.sorted.mkString(",")

    println(s"Result 2: $result")
  }

  private def parseGraph(): UndirectedGraph[String] = {
    val pattern = "([a-z]{2})-([a-z]{2})".r
    val pairs = input.map {
      case pattern(a, b) => (a, b)
    }

    pairs.foldLeft(UndirectedGraph[String]()) { case (graph, (a, b)) =>
      graph + a + b + UndirectedGraph.Edge(a, b, 0)
    }
  }
}

case object Day23 extends App {
  val input = Files.lines("2024/day23.txt")
  val problem = Day23(input)
  problem.solve1()
  problem.solve2()
}
