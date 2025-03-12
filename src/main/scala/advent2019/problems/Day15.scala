package space.scown.adventofcode
package advent2019.problems

import advent2019.intcode.{IntcodeComputer, IntcodeProgram, RequiresInput}
import lib.{BFS, Complex, Files, Problem}

case class Day15(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val program = IntcodeProgram.fromLines(input)
    val computer = IntcodeComputer(program)

    val initialState = computer.execute() match {
      case ri@RequiresInput(_, _) => State(Complex.ZERO[Int], ri)
    }

    val result = findOxygenTank(initialState).steps

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val program = IntcodeProgram.fromLines(input)
    val computer = IntcodeComputer(program)

    val initialState = computer.execute() match {
      case ri@RequiresInput(_, _) => State(Complex.ZERO[Int], ri)
    }

    val oxygenState = findOxygenTank(initialState).value

    val reachableStates = BFS.reachable(oxygenState)(expandState)
    val result = reachableStates.maxBy(_.steps).steps

    println(s"Result 2: $result")
  }

  private def findOxygenTank(initialState: State): BFS.PathState[State] = {
    BFS.solve(initialState)(_.isOxygen)((state, _) => expandState(state)).get
  }

  private def expandState(state: State): Seq[State] =
    (1L to 4L).map { d =>
      val delta = d match {
        case 1 => Complex.I[Int]
        case 2 => -Complex.I[Int]
        case 3 => -Complex.ONE[Int]
        case 4 => Complex.ONE[Int]
      }

      State(state.position + delta, state.next.continue(d) match {
        case ri@RequiresInput(_, _) => ri
      })
    }
    .filter(_.isValid)


  private case class State(position: Complex[Int], next: RequiresInput) {
    def isValid: Boolean = next.outputs.nonEmpty &&next.outputs.head != 0L
    def isOxygen: Boolean = next.outputs.nonEmpty && next.outputs.head == 2L

    override def hashCode(): Int = position.hashCode()

    override def equals(obj: Any): Boolean = obj match {
      case State(op, _) => position == op
      case _ => false
    }
  }
}

case object Day15 extends App {
  val input = Files.lines("2019/day15.txt")
  val problem = Day15(input)
  problem.solve1()
  problem.solve2()
}
