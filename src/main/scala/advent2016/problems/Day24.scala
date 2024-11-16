package space.scown.adventofcode
package advent2016.problems

import lib.UndirectedGraph.Edge
import lib._

case class Day24(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val minNumber = 0
    val maxNumber = 7

    val graph = buildGraph(minNumber, maxNumber)

    val paths = (1 to maxNumber).permutations.map(p => 0 +: p.toVector)
    val result = paths.map { path =>
      traversePath(path, graph)
    }.min

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val minNumber = 0
    val maxNumber = 7

    val graph = buildGraph(minNumber, maxNumber)

    val paths = (1 to maxNumber).permutations.map(p => 0 +: p.toVector :+ 0)
    val result = paths.map { path =>
      traversePath(path, graph)
    }.min

    println(s"Result 2: $result")
  }

  private def buildGraph(minNumber: Int, maxNumber: Int): UndirectedGraph[Int] = {
    val grid = Grid(input.map(_.split("").map(_.head).toVector))

    val interestingAddresses = grid.zipWithIndex.filter(_._1.isDigit).map(p => p.copy(_1 = p._1.asDigit)).toMap

    val nodes = interestingAddresses.keySet

    val edges = for {
      i <- minNumber until maxNumber
      j <- i + 1 to maxNumber
    } yield {
      val startAddress = interestingAddresses(i)
      val destinationAddress = interestingAddresses(j)

      val distance = BFS.solve[Complex[Int]](startAddress, state => state == destinationAddress) {
        (address, _) =>
          grid.neighbours(address)
            .filter(grid.apply(_) != '#')
      }.get.steps

      Edge(i,j, distance)
    }

    UndirectedGraph(nodes).addEdges(edges)
  }

  private def traversePath(path: Vector[Int], graph: UndirectedGraph[Int]) = {
    path.zip(path.tail).foldLeft(0)((count, pair) => {
      count + graph.edges(pair._1).find(e => e.v1 == pair._2 || e.v2 == pair._2).get.weight
    })
  }
}

case object Day24 extends App {
  val input = Files.lines("2016/day24.txt")
  val problem = Day24(input)
  Timer.time(() => problem.solve1())
  Timer.time(() => problem.solve2())
}
