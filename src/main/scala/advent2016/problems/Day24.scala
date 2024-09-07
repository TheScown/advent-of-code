package space.scown.adventofcode
package advent2016.problems

import lib.UndirectedGraph.{Edge, Node}
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

    val nodes = interestingAddresses.keySet.map(Node(_))

    val edges = for {
      i <- minNumber until maxNumber
      j <- i + 1 to maxNumber
    } yield {
      val startAddress = interestingAddresses(i)
      val destinationAddress = interestingAddresses(j)

      val distance = BFS.solve[State](State(startAddress, 0), state => state.current == destinationAddress) {
        case State(address, moves) =>
          grid.neighbours(address)
            .filter(grid.apply(_) != '#')
            .map(State(_, moves + 1))
      }.moves

      Edge(Node(i), Node(j), distance)
    }

    UndirectedGraph(nodes).addEdges(edges)
  }

  private def traversePath(path: Vector[Int], graph: UndirectedGraph[Int]) = {
    path.zip(path.tail).foldLeft(0)((count, pair) => {
      count + graph.edges(Node(pair._1)).find(e => e.v1 == Node(pair._2) || e.v2 == Node(pair._2)).get.weight
    })
  }

  case class State(current: Complex[Int], moves: Int) {
    override def equals(obj: Any): Boolean = obj match {
      case State(c, _) =>  current == c
      case _ => false
    }

    override def hashCode(): Int = current.hashCode()
  }
}

case object Day24 extends App {
  val input = Files.lines("2016/day24.txt")
  val problem = Day24(input)
  Timer.time(() => problem.solve1())
  Timer.time(() => problem.solve2())
}
