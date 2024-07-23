package space.scown.adventofcode
package lib

case object DFS {

  def reachable[T](start: T)(next: T => Seq[T]): Set[T] = {
    def helper(state: T, seen: Set[T]): Set[T] = {
      val nextStates = next(state)

      nextStates
        .filterNot(seen.contains)
        .foldLeft(seen ++ nextStates) { (seen, state) =>
          seen ++ helper(state, seen)
        }
    }

    helper(start, Set(start))
  }

}
