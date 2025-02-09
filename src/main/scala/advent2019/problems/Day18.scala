package space.scown.adventofcode
package advent2019.problems

import lib.BFS.PathState
import lib._

case class Day18(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val grid = Grid(input.map(_.toVector))
    val start = grid.indexOf('@').get
    val initialState = State(start, Set(), 0)

    val keySet = ('a' to 'z').toSet
    val keyPositions = grid.indices.filter(p => keySet.contains(grid(p))).toSet
    val keyCount = keyPositions.size
    val doorSet = ('A' to 'Z').toSet

    val dependencyGraph = buildDependencies(Vector(start), keyPositions, grid, doorSet)

    val resultState = Dijkstra.solve[State](
      initialState,
      Ordering.by[State, Int](_.steps).reverse,
      state => state.keys.size == keyCount
    ) { state =>
      val position = state.position
      val newDestinations = dependencyGraph(position)

      newDestinations.flatMap {
        case PathState(DependencyState(destination, dependencies), steps) =>
          if (!dependencies.forall(state.keys.contains)) Seq()
          else Seq(State(destination, state.keys + grid(destination), state.steps + steps))
      }.filter(_.keys.size == state.keys.size + 1)
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
    val keyPositions = grid.indices.filter(p => keySet.contains(grid(p))).toSet
    val keyCount = keyPositions.size
    val doorSet = ('A' to 'Z').toSet

    val starts = updatedGrid.zipWithIndex.filter(_._1 == '@').map(_._2).toVector
    val dependencyGraph = buildDependencies(starts, keyPositions, updatedGrid, doorSet)

    val initialState = MultiState(starts, Set(), 0)

    val resultState = Dijkstra.solve[MultiState](
      initialState,
      Ordering.by[MultiState, Int](_.steps).reverse,
      state => state.keys.size == keyCount
    ) { state =>
      state.positions.indices.flatMap { i =>
        val position = state.positions(i)
        val newDestinations = dependencyGraph(position)

        newDestinations.flatMap {
          case PathState(DependencyState(destination, dependencies), steps) =>
            if (!dependencies.forall(state.keys.contains)) Seq()
            else Seq(MultiState(state.positions.updated(i, destination), state.keys + grid(destination), state.steps + steps))
        }
      }.filter(_.keys.size == state.keys.size + 1)
    }

    val result = resultState.get.steps

    println(s"Result 2: $result")
  }

  private def buildDependencies(
    startPositions: Vector[Complex[Int]],
    keyPositions: Set[Complex[Int]],
    grid: Grid[Char],
    doorSet: Set[Char]
  ): Map[Complex[Int], Vector[PathState[DependencyState]]] = {
    (keyPositions ++ startPositions).map { initialPosition
    =>
      val initialState = DependencyState(initialPosition, Set())

      val reachableStates = BFS.reachable[DependencyState](initialState) { state =>
        val current = grid(state.position)
        val neighbours = grid.neighbours(state.position).filter(n => grid(n) != '#')

        val newDependencies = if (doorSet.contains(current)) state.dependencies + current.toLower else state.dependencies

        neighbours.map(n => DependencyState(n, newDependencies))
      }

      val validStates = reachableStates.filter(s => keyPositions.contains(s.value.position))

      initialPosition -> validStates.toVector
    }.toMap
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

  private case class DependencyState(position: Complex[Int], dependencies: Set[Char]) {
    override def hashCode(): Int = position.hashCode()

    override def equals(obj: Any): Boolean = obj match {
      case DependencyState(otherDestination, _) => position == otherDestination
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
