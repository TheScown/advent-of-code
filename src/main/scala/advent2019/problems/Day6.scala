package space.scown.adventofcode
package advent2019.problems

import lib.{BFS, Files, Problem, UndirectedGraph}

case class Day6(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val orbits = parse()

    def helper(label: String, depth: Int): Int = {
      val children = orbits.getOrElse(label, Vector())

      if (children.isEmpty) depth
      else {
        children.map(child => helper(child, depth + 1)).sum + depth
      }
    }

    val result = helper("COM", 0)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val orbits = parse()
    val graph = buildGraph(orbits)

    val startNode = graph.neighbours("YOU").head
    val result = BFS.solve[String](startNode, state => orbits.getOrElse(state, Vector()).contains("SAN")) { (state, _) =>
      graph.neighbours(state).toVector
    }.get.steps

    println(s"Result 2: $result")
  }

  private def buildGraph(orbits: Map[String, Vector[String]]): UndirectedGraph[String] = {
    val nodes = orbits.flatMap {
      case (lhs, rhs) => rhs.toSet + lhs
    }.toSet

    val initialGraph = UndirectedGraph(nodes)

    orbits.foldLeft(initialGraph) { case (graph, (lhs, rhs)) =>
      rhs.foldLeft(graph)((graph, node) => graph + UndirectedGraph.Edge(lhs, node, 1))
    }
  }

  private def parse(): Map[String, Vector[String]] = {
    val pairs = input.map { line =>
      val parts = line.split("\\)")
      (parts(0), parts(1))
    }

    pairs.groupMap(_._1)(_._2)
  }
}

case object Day6 extends App {
  val input = Files.lines("2019/day6.txt")
  val problem = Day6(input)
  problem.solve1()
  problem.solve2()
}
