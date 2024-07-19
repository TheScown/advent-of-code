package space.scown.adventofcode
package lib

import scala.annotation.tailrec
import scala.collection.mutable

case object Dijkstra {

  def solve[T](start: T, comparator: Ordering[T], goal: T => Boolean)(next: T => Seq[T]): T = {
    val queue = mutable.PriorityQueue[T]()(comparator)
    queue.enqueue(start)

    @tailrec
    def helper(): T = {
      if (queue.isEmpty) throw new IllegalStateException("No more states to test")
      else {
        val current = queue.dequeue()
        val nextStates = next(current)

        nextStates.find(goal) match {
          case Some(state) =>
            state
          case None =>
            nextStates.foreach(queue.enqueue(_))
            helper()
        }
      }
    }

    helper()
  }

}
