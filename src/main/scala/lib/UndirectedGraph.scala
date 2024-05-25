package space.scown.adventofcode
package lib

case class UndirectedGraph[T](
  nodes: Set[UndirectedGraph.Node[T]] = Set[UndirectedGraph.Node[T]](),
  edges: Map[UndirectedGraph.Node[T], Set[UndirectedGraph.Edge[T]]] = Map[UndirectedGraph.Node[T], Set[UndirectedGraph.Edge[T]]]()
) {
  type Node = UndirectedGraph.Node[T]
  type Edge = UndirectedGraph.Edge[T]

  def +(n: Node): UndirectedGraph[T] = UndirectedGraph(nodes + n, edges)
  def +(e: Edge): UndirectedGraph[T] = copy(
    edges = edges + (e.v1 -> (edges.getOrElse(e.v1, Set()) + e)) + (e.v2 -> (edges.getOrElse(e.v2, Set()) + e))
  )

  def addNodes(newNodes: Iterable[this.Node]): UndirectedGraph[T] = UndirectedGraph(nodes ++ newNodes, edges)
  def addEdges(newEdges: Iterable[this.Edge]): UndirectedGraph[T] = newEdges.foldLeft(this)((g, e) => g + e)

}

object UndirectedGraph {
  case class Node[T](value: T)
  case class Edge[T](v1: Node[T], v2: Node[T], weight: Int) {
    override def equals(obj: Any): Boolean = obj match {
      case Edge(w1, w2, weight2) => (v1 == w1 || v1 == w2) && (v2 == w1 || v2 == w2) && weight2 == weight
      case _ => false
    }

    override def hashCode(): Int = v1.hashCode() + v2.hashCode() + weight
  }
}
