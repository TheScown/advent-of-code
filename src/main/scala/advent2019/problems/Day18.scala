package space.scown.adventofcode
package advent2019.problems

import lib._

case class Day18(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val grid = Grid(input.map(_.toVector))
    val start = grid.indexOf('@').get
    val initialState = State(start, Set(), 0)

    val keySet = ('a' to 'z').toSet
    val keyCount = grid.count(keySet.contains)
    val doorSet = ('A' to 'Z').toSet

    val resultState = Dijkstra.solve[State](
      initialState,
      Ordering.by[State, Int](_.steps).reverse,
      state => state.keys.size == keyCount
    ) { state =>
      BFS.reachable(state) { bfsState =>
        val current = grid(bfsState.position)

        if (keySet.contains(current) && !state.keys.contains(current) || doorSet.contains(current) && !bfsState.keys.contains(current.toLower)) Seq()
        else {
          val neighbours = grid.neighbours(bfsState.position).filter(n => grid(n) != '#')

          neighbours.map { n =>
            val atN = grid(n)
            val newKeys = if (keySet.contains(atN)) bfsState.keys + atN else bfsState.keys
            State(n, newKeys, bfsState.steps + 1)
          }
        }
      }.map(_.value).filter(_.keys.size == state.keys.size + 1).toSeq
    }

    val result = resultState.get.steps

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val grid = Grid(input.map(_.toVector))
    val originalStart = grid.indexOf('@').get
    val neighbours = grid.neighbours(originalStart)
    val diagonals = grid.diagonals(originalStart)
    val gridWithWalls = neighbours.foldLeft(grid)((grid, n) => grid.updated(n, '#')).updated(originalStart, '#')
    val updatedGrid = diagonals.foldLeft(gridWithWalls)((grid, d) => grid.updated(d, '@'))

    val keySet = ('a' to 'z').toSet
    val keyCount = updatedGrid.count(keySet.contains)
    val doorSet = ('A' to 'Z').toSet

    val starts = updatedGrid.zipWithIndex.filter(_._1 == '@').map(_._2).toVector
    val initialState = MultiState(starts, Set(), 0)

    val resultState = Dijkstra.solve[MultiState](
      initialState,
      Ordering.by[MultiState, Int](_.steps).reverse,
      state => state.keys.size == keyCount
    ) { state =>
      state.positions.indices.flatMap { i =>
        val set = BFS.reachable(state) { bfsState =>
          val position = bfsState.positions(i)
          val current = updatedGrid(position)

          if (keySet.contains(current) && !state.keys.contains(current) || doorSet.contains(current) && !bfsState.keys.contains(current.toLower)) Seq()
          else {
            val neighbours = updatedGrid.neighbours(position).filter(n => updatedGrid(n) != '#')

            neighbours.map { n =>
              val atN = updatedGrid(n)
              val newKeys = if (keySet.contains(atN)) bfsState.keys + atN else bfsState.keys
              MultiState(bfsState.positions.updated(i, n), newKeys, bfsState.steps + 1)
            }
          }
        }
        set
      }.map(_.value).filter(_.keys.size == state.keys.size + 1)
    }

    val result = resultState.get.steps

    println(s"Result 2: $result")
  }

  private case class State(position: Complex[Int], keys: Set[Char], steps: Int) {
    override def hashCode(): Int = position.hashCode() + 37 * keys.hashCode()

    override def equals(obj: Any): Boolean = obj match {
      case State(otherPosition, otherKeys, _) => position == otherPosition && keys == otherKeys
      case _ => false
    }
  }

  private case class MultiState(positions: Vector[Complex[Int]], keys: Set[Char], steps: Int) {
    override def hashCode(): Int = positions.hashCode() + 37 * keys.hashCode()

    override def equals(obj: Any): Boolean = obj match {
      case MultiState(otherPositions, otherKeys, _) => positions == otherPositions && keys == otherKeys
      case _ => false
    }
  }
}

case object Day18 extends App {
  val input = Files.lines("2019/day18.txt")
  val problem = Day18(input)
  Timer.time(() => problem.solve1())
  Timer.time(() => problem.solve2())
}
