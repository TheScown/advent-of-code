package space.scown.adventofcode
package advent2024.problems

import lib._

case class Day16(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val grid = Grid(input.map(_.toVector))
    val start = grid.indexOf('S').get
    val goal = grid.indexOf('E').get

    val initialState = State(start, Complex.ONE, 0, 0)

    val finalState = bestPathFrom(grid, goal, initialState)

    val result = finalState.get.score

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val grid = Grid(input.map(_.toVector))
    val start = grid.indexOf('S').get
    val goal = grid.indexOf('E').get

    val initialState = State(start, Complex.ONE, 0, 0)
    val bestPath = bestPathFrom(grid, goal, initialState).get

    val validOtherPaths = BFS.reachable[State](initialState) { state =>
      val validChoices = newStates(grid, state)

      validChoices
        .filter(_.score <= bestPath.score)
        .filter { state =>
          bestPathFrom(grid, goal, state).map(_.score).getOrElse(-1) == bestPath.score
        }
    }.map(_.value)

    val allPositions = validOtherPaths.map(s => s.position)
    val result = allPositions.size

    println(s"Result 2: $result")
  }

  private def bestPathFrom(grid: Grid[Char], goal: Complex[Int], initialState: State): Option[State] = {
    Dijkstra.solve(initialState)(Ordering.by[State, Long](_.score).reverse, _.position == goal) { state =>
      newStates(grid, state)
    }
  }

  private def newStates(grid: Grid[Char], state: State) = {
    val next = grid.next(state.position, state.direction)

    val stepState = if (grid(next) == '#') Seq() else Seq(state.move(next))

    val turns = Seq(
      Complex.I[Int],
      -Complex.I[Int]
    ).flatMap { t =>
      if (grid(grid.next(state.position, state.direction * t)) == '#') Seq()
      else Seq(state.turn(state.direction * t).move(state.position + state.direction * t))
    }

    Seq(
      stepState,
      turns,
    ).flatten
  }

  case class State(position: Complex[Int], direction: Complex[Int], steps: Long, turns: Long) {
    def score: Long = 1000 * turns + steps

    def move(newPosition: Complex[Int]): State = copy(position = newPosition, steps = steps + 1)
    def turn(newDirection: Complex[Int]): State = copy(direction = newDirection, turns = turns + 1)

    override def equals(obj: Any): Boolean = obj match {
      case State(p, d, _, _) => p == position && direction == d
      case _ => false
    }

    override def hashCode(): Int = position.hashCode() + 37 * direction.hashCode()
  }
}

case object Day16 extends App {
  val input = Files.lines("2024/day16.txt")
  val problem = Day16(input)
  problem.solve1()
  Timer.time(() => problem.solve2())
}
