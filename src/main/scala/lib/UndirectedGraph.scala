package space.scown.adventofcode
package lib

import scala.annotation.tailrec

case class UndirectedGraph[T](
  nodes: Set[T] = Set[T](),
  edges: Map[T, Set[UndirectedGraph.Edge[T]]] = Map[T, Set[UndirectedGraph.Edge[T]]]()
) {
  type Edge = UndirectedGraph.Edge[T]

  def +(n: T): UndirectedGraph[T] = UndirectedGraph(nodes + n, edges)
  def +(e: Edge): UndirectedGraph[T] = copy(
    edges = edges + (e.v1 -> (edges.getOrElse(e.v1, Set()) + e)) + (e.v2 -> (edges.getOrElse(e.v2, Set()) + e))
  )

  def addNodes(newNodes: Iterable[T]): UndirectedGraph[T] = UndirectedGraph(nodes ++ newNodes, edges)
  def addEdges(newEdges: Iterable[this.Edge]): UndirectedGraph[T] = newEdges.foldLeft(this)((g, e) => g + e)

  def neighbours(node: T): Set[T] = {
    edges.getOrElse(node, Set()).flatMap(e => Set(e.v1, e.v2)) - node
  }

  // Implement Bron–Kerbosch
  def maximalCliques(): Vector[Set[T]] = {
    def getPivot(nodes: Set[T]): T = {
      nodes.maxBy(node => neighbours(node).size)
    }

    def bronKerbosch(r: Set[T], p: Set[T], x: Set[T], acc: Vector[Set[T]]): Vector[Set[T]] = {
      @tailrec
      def innerLoop(vertexSet: Set[T], r: Set[T], p: Set[T], x: Set[T], acc: Vector[Set[T]]): Vector[Set[T]] = {
        if (vertexSet.isEmpty) acc
        else {
          val v = vertexSet.head
          val updatedAcc = bronKerbosch(r + v, p intersect neighbours(v), x intersect neighbours(v), acc)
          innerLoop(vertexSet - v, r, p - v, x + v, updatedAcc)
        }
      }

      if (p.isEmpty && x.isEmpty) acc :+ r
      else {
        val testSet = p union x
        val pivot = getPivot(testSet)
        val vertexSet = p -- neighbours(pivot)
        innerLoop(vertexSet, r, p, x, acc)
      }
    }

    bronKerbosch(Set(), edges.keySet, Set(), Vector())
  }
}

object UndirectedGraph {
  case class Edge[T](v1: T, v2: T, weight: Int) {
    override def equals(obj: Any): Boolean = obj match {
      case Edge(w1, w2, weight2) => (v1 == w1 || v1 == w2) && (v2 == w1 || v2 == w2) && weight2 == weight
      case _ => false
    }

    override def hashCode(): Int = v1.hashCode() + v2.hashCode() + weight
  }
}
