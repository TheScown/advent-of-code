package space.scown.advent2023
package problems

import lib.Timer.time
import lib.{Files, Problem}

import scala.annotation.tailrec
import scala.util.Random

case class Day25(lines: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val graph = parse
    val edgeCount = graph.edgeCount

    println(s"Graph size: ${graph.nodes.size}, $edgeCount")

    @tailrec
    def helper(modifiedGraph: Day25Graph, attempt: Int, startTime: Long): Day25Graph = {
      if (modifiedGraph.nodes.size == 2) {
        println(s"Duration: ${System.currentTimeMillis() - startTime}ms")

        if (modifiedGraph.edgeCount == 3) {
          println(s"Attempt: $attempt, Correct cut found")
          modifiedGraph
        }
        else {
          println(s"Attempt: $attempt, Cut of size ${modifiedGraph.edgeCount} found - retrying")
          helper(graph, attempt + 1, System.currentTimeMillis())
        }
      }
      else {
        helper(modifiedGraph - modifiedGraph.randomEdge(), attempt, startTime)
      }
    }

    val contractedGraph = helper(graph, 1, System.currentTimeMillis())

    contractedGraph.nodes.foreach(println)
    println(contractedGraph.edges.size)

    val result = contractedGraph.nodes.map(_.label.size).product

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    println("Just push the Big Red Button!")
  }

  private def parse: Day25Graph = {
    @tailrec
    def helper(remainingLines: Vector[String], graph: Day25Graph): Day25Graph = {
      if (remainingLines.isEmpty) graph
      else {
        val line = remainingLines.head
        val parts = line.split(":")
        val lhs = parts(0)
        val edges = parts(1).trim.split(" ")

        val newNodes = (lhs :: edges.toList).map(s => Day25Node(Set(s)))
        val newEdges = newNodes.tail.map(n => Day25Edge(newNodes.head, n))

        helper(remainingLines.tail, (graph ++ newNodes).addEdges(newEdges))
      }
    }

    helper(lines, Day25Graph(Set(), Map()))
  }
}

case class Day25Graph(nodes: Set[Day25Node], edges: Map[Day25Node, Vector[Day25Edge]]) {

  private val random = new Random()

  def +(node: Day25Node): Day25Graph = copy(nodes = nodes + node)
  def ++(newNodes: Seq[Day25Node]): Day25Graph = copy(nodes = nodes ++ newNodes)
  def addEdges(newEdges: Seq[Day25Edge]): Day25Graph = newEdges.foldLeft(this)((g, e) => g + e)

  def +(edge: Day25Edge): Day25Graph = copy(
    edges = edges + (edge.v1 -> (edges.getOrElse(edge.v1, Vector()) :+ edge)) + (edge.v2 -> (edges.getOrElse(edge.v2, Vector()) :+ edge))
  )

  lazy val edgeCount: Int = edges.values.map(edges => edges.size).sum / 2

  def randomEdge(): Day25Edge = {
    val randomNode = nodes.toVector(random.nextInt(nodes.size))
    val edgesForRandomNode = edges(randomNode)
    edgesForRandomNode(random.nextInt(edgesForRandomNode.size))
  }

  def -(edge: Day25Edge): Day25Graph = {
    if ((edge.v1.label intersect edge.v2.label).nonEmpty) {
      throw new IllegalStateException(s"We've combined nodes twice: ${edge.v1.label} and ${edge.v2.label}")
    }

    val newNode = Day25Node(edge.v1.label ++ edge.v2.label)

    val matchingEdges = edges(edge.v1) ++ edges(edge.v2)
    val edgesToChange = matchingEdges.filterNot(e => e == edge)

    val newEdges = edgesToChange.map { e =>
      if (e.v1 == edge.v1 || e.v1 == edge.v2) Day25Edge(newNode, e.v2)
      else Day25Edge(e.v1, newNode)
    }

    val groupedNewEdges = newEdges.groupBy(e => if (e.v1 == newNode) e.v2 else e.v1)
    val updatedEntries = groupedNewEdges
      .map({
        case (updatedNode, updatedEdges) => updatedNode -> (edges(updatedNode).filterNot(e => edgesToChange.contains(e)) ++ updatedEdges)
      })

    val updatedEdges = edges - edge.v1 - edge.v2 + (newNode -> newEdges) ++ updatedEntries
    copy(
      nodes = nodes - edge.v1 - edge.v2 + newNode,
      edges = updatedEdges
    )
  }

}

case class Day25Node(label: Set[String])

case class Day25Edge(v1: Day25Node, v2: Day25Node) extends Equals {

  if (v1 == v2) {
    throw new IllegalStateException(s"Reflexive edge created on $v1")
  }

  override def equals(obj: Any): Boolean = obj match {
    case Day25Edge(w1, w2) => (v1 == w1 || v1 == w2) && (v2 == w1 || v2 == w2)
    case _ => false
  }

  override def hashCode(): Int = v1.hashCode() + v2.hashCode()

}

object Day25 {
  def main(args: Array[String]): Unit = {
    val value = Files.lines("day25.txt")
    time(() => Day25(value).solve1())
    time(() => Day25(value).solve2())
  }

}
