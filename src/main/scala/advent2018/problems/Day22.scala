package space.scown.adventofcode
package advent2018.problems

import lib._

import scala.math.Numeric.IntIsIntegral

case class Day22(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val (depth, target) = parse()
    val terrain = computeTerrain(depth, target)

    val result = terrain.foldLeft(0L)((acc, value) => acc + value)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val (depth, target) = parse()
    val terrain = computeTerrain(depth, target, 5)

    val isGoal = (state: State) => state.position == target && state.tool == Torch
    val startState = State(Complex.ZERO, Torch, 0, Vector(), Map())
    val comparator: Ordering[State] = Ordering.by[State, Int](-_.score(target))

    val resultState = Dijkstra.solve(startState, comparator, isGoal) { state =>
      val currentTerrain = terrain(state.position)

      val moveStates = terrain.neighbours(state.position)
        .filter { position =>
          validTools(terrain(position)).contains(state.tool)
        }
        .map(position => state.copy(
          position = position,
          time = state.time + 1,
          history = state.history :+ state.position
        ))

      val changeToolState = state.copy(
        tool = (validTools(currentTerrain) - state.tool).head,
        time = state.time + 7,
        history = state.history :+ state.position
      )

      moveStates :+ changeToolState
    }

    val result = resultState.time

    println(s"Result 2: $result")
  }

  private def computeTerrain(depth: Int, target: Complex[Int], scaleFactor: Int = 1): Grid[Int] = {
    val emptyGrid = Grid.of((-target.im + 1) * scaleFactor, (target.re + 1) * scaleFactor, 0)

    val erosionLevels = emptyGrid.indices.foldLeft(emptyGrid) { (acc, index) =>
      val geologicIndex = if (index == Complex.ZERO(IntIsIntegral) || index == target) 0
      else if (index.isReal) index.re * 16807
      else if (index.isImaginary) -index.im * 48271
      else acc(index - Complex.ONE) * acc(index + Complex.I)

      acc.updated(index, (geologicIndex + depth) % 20183)
    }

    erosionLevels.map(_ % 3)
  }

  private def parse(): (Int, Complex[Int]) = {
    val depthPattern = "depth: (\\d+)".r
    val targetPattern = "target: (\\d+),(\\d+)".r

    val depth = input(0) match {
      case depthPattern(depth) => depth.toInt
    }

    val target = input(1) match {
      case targetPattern(x, y) => Complex(x.toInt, -y.toInt)
    }

    (depth, target)
  }

  private def validTools(terrain: Int): Set[Tool] = terrain match {
    case 0 => Set(ClimbingGear, Torch)
    case 1 => Set(ClimbingGear, NoTool)
    case 2 => Set(Torch, NoTool)
  }

  private trait Tool
  private case object ClimbingGear extends Tool
  private case object Torch extends Tool
  private case object NoTool extends Tool

  private case class State(
    position: Complex[Int],
    tool: Tool,
    time: Int,
    history: Vector[Complex[Int]],
    cache: Map[Complex[Int], (Int, Int)]
  ) {
    def score(target: Complex[Int]): Int = {
      val cost = time
      val remainingSteps = position mh target
      cost + remainingSteps
    }

    override def hashCode(): Int = position.hashCode() + 37 * tool.hashCode()

    override def equals(obj: Any): Boolean = obj match {
      case State(otherPosition, otherTool, _, _, _) => otherPosition == position && otherTool == tool
      case _ => false
    }
  }
}

case object Day22 extends App {
  val input = Files.lines("2018/day22.txt")
  val problem = Day22(input)
  problem.solve1()
  Timer.time(() => problem.solve2())
}
