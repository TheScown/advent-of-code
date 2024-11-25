package space.scown.adventofcode
package lib

import scala.annotation.tailrec
import scala.collection.immutable.Queue

case object BFS {

  case class PathState[T](value: T, steps: Int) {
    override def hashCode(): Int = value.hashCode()

    override def equals(obj: Any): Boolean = obj match {
      case PathState(otherValue, _) => otherValue == value
      case _ => false
    }
  }

  def solve[T](start: T, goal: T => Boolean)(next: (T, Int) => Seq[T]): Option[PathState[T]] = {
    @tailrec
    def helper(queue: Queue[PathState[T]], seen: Set[T]): Option[PathState[T]] = {
      if (queue.isEmpty)
        None
      else {
        val (current, nextQueue) = queue.dequeue
        val nextStates = next(current.value, current.steps).filterNot(seen.contains)

        nextStates.find(goal) match {
          case Some(state) => Some(PathState(state, current.steps + 1))
          case None =>
            val updatedQueue = nextQueue.enqueueAll(nextStates.map(PathState(_, current.steps + 1)))
            helper(updatedQueue, seen ++ nextStates)
        }
      }
    }

    if (goal(start)) Some(PathState(start, 0))
    else helper(Queue(PathState(start, 0)), Set(start))
  }

  def reachable[T](start: T)(next: T => Seq[T]): Set[PathState[T]] = {
    @tailrec
    def helper(queue: Queue[PathState[T]], seen: Set[PathState[T]]): Set[PathState[T]] = {
      if (queue.isEmpty) seen
      else {
        val (current, nextQueue) = queue.dequeue
        val nextStates = next(current.value).map(v => PathState(v, current.steps + 1)).filterNot(seen.contains)

        val updatedQueue = nextQueue.enqueueAll(nextStates)
        helper(updatedQueue, seen ++ nextStates)
      }
    }

    val initialState = PathState(start, 0)
    helper(Queue(initialState), Set(initialState))
  }

}
