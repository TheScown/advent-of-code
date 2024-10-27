package space.scown.adventofcode
package lib

import scala.annotation.tailrec

case class DirectedGraph[T](
  nodes: Set[T] = Set[T](),
  edges: Map[T, Set[DirectedGraph.Edge[T]]] = Map[T, Set[DirectedGraph.Edge[T]]]()
) {
  type Edge = DirectedGraph.Edge[T]

  def +(n: T): DirectedGraph[T] = DirectedGraph(nodes + n, edges)
  def +(e: Edge): DirectedGraph[T] = copy(
    edges = edges + (e.v1 -> (edges.getOrElse(e.v1, Set()) + e))
  )

  def addEdges(newEdges: Iterable[this.Edge]): DirectedGraph[T] = newEdges.foldLeft(this)((g, e) => g + e)

  def neighbours(node: T): Set[T] = {
    edges.getOrElse(node, Set()).flatMap(e => Set(e.v1, e.v2)) - node
  }

  def tarjan(): Vector[Set[T]] = {
    case class State(index: Int, lowLink: Int, onStack: Boolean)
    var index = 0

    def strongConnect(v: T, stack: List[T], stateMap: Map[T, State], acc: Vector[Set[T]]): (List[T], Map[T, State], Vector[Set[T]]) = {
      val state = State(index, index, onStack = true)
      index = index + 1
      val updatedStack = v :: stack
      val updatedStateMap = stateMap + (v -> state)

      val (stackAfterChildren, stateMapAfterChildren, accAfterChildren) = edges(v).foldLeft((updatedStack, updatedStateMap, acc)) { case ((stack, stateMap, acc), e) =>
        val w = e.v2

        if (!stateMap.contains(w)) {
          val (wStack, wStateMap, wAcc) = strongConnect(w, stack, stateMap, acc)
          val vLowLink = wStateMap(v).lowLink
          val wLowLink = wStateMap(w).lowLink
          val finalStateMap = wStateMap + (v -> wStateMap(v).copy(lowLink = math.min(vLowLink, wLowLink)))
          (wStack, finalStateMap, wAcc)
        }
        else if (stateMap(w).onStack) {
          val vLowLink = stateMap(v).lowLink
          val wIndex = stateMap(w).index
          val finalStateMap = stateMap + (v -> stateMap(v).copy(lowLink = math.min(vLowLink, wIndex)))
          (stack, finalStateMap, acc)
        }
        else (stack, stateMap, acc)
      }

      val finalVState = stateMapAfterChildren(v)

      if (finalVState.index == finalVState.lowLink) {
        // Note we also need to pop v, which is the head of nextStack
        val (componentNodes, nextStack) = stackAfterChildren.span(_ != v)
        val componentNodeSet = componentNodes.toSet + nextStack.head
        val nextStateMap = stateMapAfterChildren ++ componentNodeSet.map(node => node -> stateMapAfterChildren(node).copy(onStack = false))

        (nextStack.tail, nextStateMap, accAfterChildren :+ componentNodeSet)
      }
      else (stackAfterChildren, stateMapAfterChildren, accAfterChildren)
    }

    @tailrec
    def helper(remainingNodes: Set[T], stack: List[T], stateMap: Map[T, State], acc: Vector[Set[T]]): Vector[Set[T]] = {
      if (remainingNodes.isEmpty) acc
      else {
        val v = remainingNodes.head

        if (stateMap.contains(v)) helper(remainingNodes - v, stack, stateMap, acc)
        else {
          val (updatedStack, updatedStateMap, updatedAcc) = strongConnect(v, stack, stateMap, acc)
          helper(remainingNodes - v, updatedStack, updatedStateMap, updatedAcc)
        }
      }
    }

    helper(nodes, Nil, Map(), Vector())
  }
}

object DirectedGraph {
  case class Edge[T](v1: T, v2: T, weight: Int)
}
