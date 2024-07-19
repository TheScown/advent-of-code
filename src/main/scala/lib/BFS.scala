package space.scown.adventofcode
package lib

import scala.annotation.tailrec
import scala.collection.immutable.Queue

case object BFS {

  def solve[T](start: T, goal: T => Boolean)(next: T => Seq[T]): T = {
    @tailrec
    def helper(queue: Queue[T], seen: Set[T]): T = {
      if (queue.isEmpty) throw new IllegalStateException("No more states to test")
      else {
        val (current, nextQueue) = queue.dequeue
        val nextStates = next(current).filterNot(seen.contains)

        nextStates.find(goal) match {
          case Some(state) => state
          case None =>
            val updatedQueue = nextQueue.enqueueAll(nextStates)
            helper(updatedQueue, seen ++ nextStates)
        }
      }
    }

    helper(Queue(start), Set(start))
  }

}
