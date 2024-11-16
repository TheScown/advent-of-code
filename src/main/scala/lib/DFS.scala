package space.scown.adventofcode
package lib

case object DFS {

  case class PathState[T](value: T, steps: Int)

  def reachable[T](start: T)(next: (T, Int) => Seq[T]): Set[T] = {
    def helper(current: PathState[T], seen: Set[T]): Set[T] = {
      val nextStates = next(current.value, current.steps)

      nextStates
        .filterNot(seen.contains)
        .foldLeft(seen ++ nextStates) { (seen, state) =>
          seen ++ helper(PathState(state, current.steps + 1), seen)
        }
    }

    helper(PathState(start, 0), Set(start))
  }

}
