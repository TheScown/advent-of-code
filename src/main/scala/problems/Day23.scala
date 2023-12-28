package space.scown.advent2023
package problems

import lib.Timer.time
import lib.{Files, Problem}

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet
import scala.collection.immutable.Queue

case class Day23(grid: Vector[Vector[Char]]) extends Problem {
  override def solve1(): Unit = {
    val graph = parse1

    val startAddress = (0, 1)
    val start = Node(startAddress)
    val target = (grid.size - 1, grid(0).size - 2)

    @tailrec
    def helper(queue: Queue[Day23Part1QueueEntry], currentLongest: Int): Int = {
      if (queue.isEmpty) currentLongest
      else {
        val (entry, nextQueue) = queue.dequeue

        if (entry.node.id == target) helper(nextQueue, if (currentLongest > entry.score) currentLongest else entry.score)
        else {
          val edges = graph.edges(entry.node)

          val updatedQueue = edges.foldLeft(nextQueue) { (queue, edge) =>
            queue.enqueue(Day23Part1QueueEntry(edge.target, entry.score + edge.weight))
          }

          helper(updatedQueue, currentLongest)
        }
      }
    }

    val result = helper(Queue(Day23Part1QueueEntry(start, 0)), 0)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val graph = parse2

    val startAddress = (0, 1)
    val start = Node(startAddress)
    val target = (grid.size - 1, grid(0).size - 2)

    @tailrec
    def helper(queue: Queue[Day23Part2QueueEntry], currentLongest: Int): Int = {
      if (queue.isEmpty) currentLongest
      else {
        val (entry, nextQueue) = queue.dequeue

        if (entry.node.id == target) helper(nextQueue, if (currentLongest > entry.score) currentLongest else entry.score)
        else {
          val edges = graph.edges(entry.node)

          val updatedQueue = edges
            .filterNot(edge => entry.path.contains(edge.target))
            .foldLeft(nextQueue) { (queue, edge) =>
              queue.enqueue(Day23Part2QueueEntry(edge.target, entry.score + edge.weight, entry.path :+ entry.node))
            }

          helper(updatedQueue, currentLongest)
        }
      }
    }

    val result = helper(Queue(Day23Part2QueueEntry(start, 0, Vector())), 0)

    println(s"Result 2: $result")
  }

  private def parse1: Graph = {
    val startAddress = (0, 1)
    val start = Node(startAddress)
    val target = (grid.size - 1, grid(0).size - 2)

    @tailrec
    def helper(queue: Queue[Day23QueueEntry], graph: Graph): Graph = {
      if (queue.isEmpty) graph
      else {
        val (entry, nextQueue) = queue.dequeue
        val (row, column) = entry.current

        if (entry.current == target) {
          // We've reached the end, no more walking to do
          val updatedGraph = graph + Node(target) + Edge(entry.previousNode, Node(target), entry.distance)
          helper(nextQueue, updatedGraph)
        }
        else {
          val nextAddresses = Vector(
            (row - 1, column),
            (row + 1, column),
            (row, column - 1),
            (row, column + 1)
          )
            .filterNot({
              case (r, c) => r < 0 || r >= grid.size || c < 0 || c >= grid(0).size
            })
            .filterNot({
              case (r, c) => grid(r)(c) == '#'
            })
            .filterNot(a => entry.previous.isDefined && a == entry.previous.get)

          if (nextAddresses.size > 1) {
            // Junction – create a new node and branch, making sure we allow for slopes
            val newNode = Node(entry.current)
            if (graph.nodes.contains(newNode)) {
              // We've already been here, add current edge but no new work
              val updatedGraph = graph + Edge(entry.previousNode, newNode, entry.distance)
              helper(nextQueue, updatedGraph)
            }
            else {
              // Add the node to the graph and figure out next steps
              val updatedGraph = graph + newNode + Edge(entry.previousNode, newNode, entry.distance)

              val updatedQueue = nextAddresses
                .filterNot({
                  case (r, c) if c == column + 1 => grid(r)(c) == '<'
                  case (r, c) if c == column - 1 => grid(r)(c) == '>'
                  case (r, c) if r == row + 1 => grid(r)(c) == '^'
                  case (r, c) if r == row - 1 => grid(r)(c) == 'v'
                })
                .foldLeft(nextQueue)((queue, address) => queue.enqueue(Day23QueueEntry(address, Some(entry.current), newNode, 1)))

              helper(updatedQueue, updatedGraph)
            }
          }
          else {
            // Path – continue
            val updatedQueue = nextQueue.enqueue(Day23QueueEntry(nextAddresses.head, Some(entry.current), entry.previousNode, entry.distance + 1))
            helper(updatedQueue, graph)
          }
        }
      }
    }

    helper(Queue(Day23QueueEntry(startAddress, None, start, 0)), Graph(SortedSet(start)(Ordering.by(_.id)), Map()))
  }

  private def parse2: Graph = {
    val startAddress = (0, 1)
    val start = Node(startAddress)
    val target = (grid.size - 1, grid(0).size - 2)

    @tailrec
    def helper(queue: Queue[Day23QueueEntry], graph: Graph): Graph = {
      if (queue.isEmpty) graph
      else {
        val (entry, nextQueue) = queue.dequeue
        val (row, column) = entry.current

        if (entry.current == target) {
          // We've reached the end, no more walking to do
          val updatedGraph = graph + Node(target) + Edge(entry.previousNode, Node(target), entry.distance)
          helper(nextQueue, updatedGraph)
        }
        else {
          val nextAddresses = Vector(
            (row - 1, column),
            (row + 1, column),
            (row, column - 1),
            (row, column + 1)
          )
            .filterNot({
              case (r, c) => r < 0 || r >= grid.size || c < 0 || c >= grid(0).size
            })
            .filterNot({
              case (r, c) => grid(r)(c) == '#'
            })
            .filterNot(a => entry.previous.isDefined && a == entry.previous.get)

          if (nextAddresses.size > 1) {
            // Junction – create a new node and branch, making sure we allow for slopes
            val newNode = Node(entry.current)
            if (graph.nodes.contains(newNode)) {
              // We've already been here, add current edge but no new work
              val updatedGraph = graph + Edge(entry.previousNode, newNode, entry.distance) + Edge(newNode, entry.previousNode, entry.distance)
              helper(nextQueue, updatedGraph)
            }
            else {
              // Add the node to the graph and figure out next steps
              val updatedGraph = graph + newNode + Edge(entry.previousNode, newNode, entry.distance) + Edge(newNode, entry.previousNode, entry.distance)

              val updatedQueue = nextAddresses
                .foldLeft(nextQueue)((queue, address) => queue.enqueue(Day23QueueEntry(address, Some(entry.current), newNode, 1)))

              helper(updatedQueue, updatedGraph)
            }
          }
          else {
            // Path – continue
            val updatedQueue = nextQueue.enqueue(Day23QueueEntry(nextAddresses.head, Some(entry.current), entry.previousNode, entry.distance + 1))
            helper(updatedQueue, graph)
          }
        }
      }
    }

    helper(Queue(Day23QueueEntry(startAddress, None, start, 0)), Graph(SortedSet(start)(Ordering.by(_.id)), Map()))
  }
}

case class Day23QueueEntry(current: (Int, Int), previous: Option[(Int, Int)], previousNode: Node, distance: Int)

case class Day23Part1QueueEntry(node: Node, score: Int)
case class Day23Part2QueueEntry(node: Node, score: Int, path: Vector[Node])

case class Node(id: (Int, Int))
case class Edge(source: Node, target: Node, weight: Int)

case class Graph(nodes: SortedSet[Node], edges: Map[Node, Set[Edge]]) {

  def +(node: Node): Graph = copy(nodes = nodes + node)

  def +(edge: Edge): Graph = {
    copy(edges = edges + (edge.source -> (edges.getOrElse(edge.source, Set()) + edge)))
  }

}

object Day23 {
  def main(args: Array[String]): Unit = {
    val value = Files.grid("day23.txt")
    time(() => Day23(value).solve1())
    time(() => Day23(value).solve2())
  }

}
