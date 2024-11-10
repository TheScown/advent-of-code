package space.scown.adventofcode
package lib

import scala.annotation.tailrec
import scala.collection.mutable

case class UnionFind(size: Int) {

  private val parents: mutable.IndexedSeq[Int] = mutable.IndexedSeq.range(0, size)
  private val sizes: mutable.IndexedSeq[Int] = mutable.IndexedSeq.fill(size)(1)

  def find(x: Int): Int = {
    @tailrec
    def helper(node: Int): Int = {
      val parent = parents(node)

      if (parent == node) node
      else {
        val grandparent = parents(parents(x))
        parents(x) = grandparent
        helper(parent)
      }
    }

    helper(x)
  }

  def union(x: Int, y: Int): Unit = {
    val xRoot = find(x)
    val yRoot = find(y)

    if (xRoot != yRoot) {
      val (smallRoot, largeRoot) = if (sizes(xRoot) < sizes(yRoot)) (xRoot, yRoot) else (yRoot, xRoot)

      parents(smallRoot) = largeRoot
      sizes(largeRoot) = sizes(largeRoot) + sizes(smallRoot)
    }
  }

  def size(x: Int): Int = sizes(find(x))

  def componentCount: Int = parents.indices.map(find).toSet.size

}

case object UnionFind {

}
