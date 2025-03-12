package space.scown.adventofcode
package lib

import scala.annotation.tailrec
import scala.collection.mutable

case object Dijkstra {

  def solve[T](start: T)(comparator: Ordering[T], goal: T => Boolean)(next: T => Seq[T]): Option[T] = {
    val queue = mutable.PriorityQueue[T]()(comparator)
    queue.enqueue(start)

    @tailrec
    def helper(seen: Set[T]): Option[T] = {
      if (queue.isEmpty) None
      else {
        val current = queue.dequeue()

        if (seen.contains(current)) helper(seen)
        else if (goal(current)) Some(current)
        else {
          val nextStates = next(current)
          nextStates.foreach(queue.enqueue(_))
          helper(seen + current)
        }
      }
    }

    helper(Set())
  }

}
