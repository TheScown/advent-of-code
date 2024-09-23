package space.scown.adventofcode
package lib

import scala.annotation.tailrec
import scala.collection.immutable.Queue

case object BFS {

  def solve[T](start: T, goal: T => Boolean)(next: T => Seq[T]): Option[T] = {
    @tailrec
    def helper(queue: Queue[T], seen: Set[T]): Option[T] = {
      if (queue.isEmpty)
        None
      else {
        val (current, nextQueue) = queue.dequeue
        val nextStates = next(current).filterNot(seen.contains)

        nextStates.find(goal) match {
          case Some(state) => Some(state)
          case None =>
            val updatedQueue = nextQueue.enqueueAll(nextStates)
            helper(updatedQueue, seen ++ nextStates)
        }
      }
    }

    if (goal(start)) Some(start)
    else helper(Queue(start), Set(start))
  }

  def reachable[T](start: T)(next: T => Seq[T]): Set[T] = {
    @tailrec
    def helper(queue: Queue[T], seen: Set[T]): Set[T] = {
      if (queue.isEmpty) seen
      else {
        val (current, nextQueue) = queue.dequeue
        val nextStates = next(current).filterNot(seen.contains)

        val updatedQueue = nextQueue.enqueueAll(nextStates)
        helper(updatedQueue, seen ++ nextStates)
      }
    }

    helper(Queue(start), Set(start))
  }

}
