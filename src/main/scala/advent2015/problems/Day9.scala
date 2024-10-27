package space.scown.adventofcode
package advent2015.problems

import lib.{Files, Problem, UndirectedGraph}

import scala.annotation.tailrec

case class Day9(input: Vector[String]) extends Problem {
  private val graph = parse()
  type Edge = UndirectedGraph.Edge[String]

  override def solve1(): Unit = {
    @tailrec
    def helper(currentNode: String, path: Vector[Edge], remainingNodes: Set[String]): Int = {
      if (remainingNodes.isEmpty)
        path.map(_.weight).sum
      else {
        val validEdges = graph.edges(currentNode).filter(e => remainingNodes(e.v1) || remainingNodes(e.v2))
        val nextEdge = validEdges.minBy(_.weight)
        val nextNode = if (nextEdge.v1 == currentNode) nextEdge.v2 else nextEdge.v1
        helper(nextNode, path :+ nextEdge, remainingNodes - nextNode)
      }
    }

    val result = graph.nodes.map(n => helper(n, Vector(), graph.nodes - n)).min

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val allEdges = graph.edges.values.reduce(_++_).toVector
    val combinations = allEdges.combinations(graph.nodes.size - 1)

    val validCombinations = combinations.filter { edges =>
      val nodes = edges.foldLeft(Vector[String]()) { (vec, edge) => vec :+ edge.v1 :+ edge.v2 }
      val counts = nodes.groupBy(n => nodes.count(n1 => n1 == n))
      counts.getOrElse(1, Vector()).toSet.size == 2 && counts.getOrElse(2, Vector()).toSet.size == graph.nodes.size - 2
    }

    val result = validCombinations.maxBy(_.map(_.weight).sum)

    println(s"Result 2: ${result.map(_.weight).sum}")
  }

  private def parse(): UndirectedGraph[String] = {
    val pattern = "(\\w+) to (\\w+) = (\\d+)".r

    input.foldLeft(UndirectedGraph[String]()) { (graph, line) =>
      val matches = pattern.findAllIn(line)
      val v1 = matches.group(1)
      val v2 = matches.group(2)
      val weight = matches.group(3).toInt

      graph + v1 + v2 + UndirectedGraph.Edge(v1, v2, weight)
    }
  }
}

case object Day9 extends App {
  val input = Files.lines("2015/day9.txt")
  val problem = Day9(input)
  problem.solve1()
  problem.solve2()
}
