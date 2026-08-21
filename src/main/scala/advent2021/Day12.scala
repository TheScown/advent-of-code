package space.scown.adventofcode
package advent2021

import lib.UndirectedGraph.Edge
import lib._

case class Day12(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val graph = parse()

    val result = countPaths(graph) { path => n =>
      !path.contains(n)
    }

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val graph = parse()

    val result = countPaths(graph) { path => n =>
      if (n.label == "start") false
      else if (!path.contains(n)) true
      else {
        val visitedSmallCaves = path.filter(_.isLargeCave == false)
        val smallCaveCounts = visitedSmallCaves.groupBy(_.label).values.map(_.size).toSet

        !smallCaveCounts.contains(2)
      }
    }

    println(s"Result 2: $result")
  }

  private def countPaths(graph: UndirectedGraph[Node])(smallNeighboursFilter: Vector[Node] => Node => Boolean): Int = {
    val allPaths = BFS.reachable(Vector(Node("start"))) { path =>
      val location = path.last

      if (location == Node("end")) Seq()
      else {
        val neighbours = graph.neighbours(location).toVector
        val (bigNeighbours, smallNeighbours) = neighbours.partition(_.isLargeCave)
        val validSmallNeighbours = smallNeighbours.filter(smallNeighboursFilter(path))

        bigNeighbours.map(next => path :+ next) ++ validSmallNeighbours.map(next => path :+ next)
      }
    }

    allPaths.count(_.value.last == Node("end"))
  }

  private def parse(): UndirectedGraph[Node] = {
    val graph = UndirectedGraph[Node]()
    val linePattern = "([A-Za-z]+)-([A-Za-z]+)".r

    input.foldLeft(graph) { (graph, line) =>
      line match {
        case linePattern(lhs, rhs) =>
          val leftNode = Node(lhs)
          val rightNode = Node(rhs)
          val edge = Edge(leftNode, rightNode, 0)

          graph + leftNode + rightNode + edge
      }
    }
  }

  private case class Node(label: String) {
    val isLargeCave: Boolean = label.forall(_.isUpper)
  }
}

case object Day12 extends App {
  val input = Files.lines("2021/day12.txt")
  val problem = Day12(input)
  problem.solve1()
  Timer.time(() => problem.solve2())
}
