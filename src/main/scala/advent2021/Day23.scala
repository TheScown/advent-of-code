package space.scown.adventofcode
package advent2021

import lib.{Complex, DFS, Dijkstra, Files, Grid, Problem, Timer}

case class Day23(input: Vector[String]) extends Problem {
  private val AMPHIPODS = Set('A', 'B', 'C', 'D')

  override def solve1(): Unit = {
    val paddedLines = Grid.padLines(input)
    val grid = Grid(paddedLines.map(_.toVector)).zipWithIndex

    val binDepth = 2
    val finalState = solve(grid, binDepth)

    val result = finalState.movementCost

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val (before, after) = input.splitAt(3)
    val fullLines = before ++ Vector(
      "  #D#C#B#A#",
      "  #D#B#A#C#"
    ) ++ after
    val paddedLines = Grid.padLines(fullLines)

    val grid = Grid(paddedLines.map(_.toVector)).zipWithIndex
    val binDepth = 4

    val finalState = solve(grid, binDepth)

    val result = finalState.movementCost

    println(s"Result 2: $result")
  }

  private def solve(grid: Grid[(Char, Complex[Int])], binDepth: Int): State = {
    val initialState = State(grid, 0)

    def score(state: State): Int = {
      state.movementCost
    }

    def validMoveToCorrectBin(state: State, amphipod: Char, amphipodTarget: Complex[Int], a: Complex[Int]): Boolean = {
      val correctBin = a.re == amphipodTarget.re
      val isBinFull = binFull(a, amphipod, binDepth, state.grid)
      correctBin && isBinFull
    }

    Dijkstra.solve(initialState)(Ordering.by[State, Int](score).reverse, isGoal) { state =>
      val amphipodNodes = state.grid.filter { case (amphipod, _) =>
        AMPHIPODS.contains(amphipod)
      }

      amphipodNodes.flatMap { case (amphipod, address) =>
        val amphipodTarget = target(amphipod)
        val inPosition = address.re == amphipodTarget.re && binFull(address, amphipod, binDepth, state.grid)

        if (inPosition) Seq()
        else {
          val reachableDots = DFS.reachable(address) { (a, _) =>
            state.grid.neighbours(a).filter(n => state.grid(n)._1 == '.')
          }

          val reachableWithoutBanned = reachableDots.filterNot(a => isBanned(state.grid, a))

          val validMoves = (reachableWithoutBanned
            .filter { a =>
              if (isBin(address)) {
                a.im == -1 || validMoveToCorrectBin(state, amphipod, amphipodTarget, a)
              }
              else if (address.im == -1) {
                validMoveToCorrectBin(state, amphipod, amphipodTarget, a)
              }
              else true
            } - address).toVector

          val binMoves = validMoves.exists(isBin)

          val finalValidMoves = if (binMoves) validMoves.filter(isBin) else validMoves

          finalValidMoves.map { a =>
            val costOfMove = state.movementCost + distance(address, a) * cost(amphipod)
            State(state.grid.updated(a, (amphipod, a)).updated(address, ('.', address)), costOfMove)
          }
        }
      }
    }.get
  }

  private def isGoal(state: State): Boolean = {
    val amphipodNodes = state.grid.filter { case (amphipod, _) =>
      AMPHIPODS.contains(amphipod)
    }

    amphipodNodes.forall { case (amphipod, address) =>
      address.re == target(amphipod).re && address.im != -1
    }
  }

  private def isBanned(grid: Grid[(Char, Complex[Int])], address: Complex[Int]) = {
    address.im == -1 && grid(address - Complex.I)._1 != '#'
  }

  private def cost(amphipod: Char): Int = amphipod match {
    case 'A' => 1
    case 'B' => 10
    case 'C' => 100
    case 'D' => 1000
  }

  private def target(amphipod: Char): Complex[Int] = amphipod match {
    case 'A' => Complex(3, -2)
    case 'B' => Complex(5, -2)
    case 'C' => Complex(7, -2)
    case 'D' => Complex(9, -2)
  }

  private def distance(a: Complex[Int], b: Complex[Int]): Int = {
    val baseDistance = a mh b
    val adjustment = if (isBin(a) && isBin(b)) 2 * (-1 - Math.max(a.im, b.im)) else 0

    baseDistance + adjustment
  }

  private def binFull(start: Complex[Int], amphipod: Char, binDepth: Int, grid: Grid[(Char, Complex[Int])]): Boolean = {
    val imsToTest = (start.im - 1) to (-1 - binDepth) by -1
    imsToTest.forall(im => grid(Complex(start.re, im))._1 == amphipod)
  }

  private def isBin(address: Complex[Int]): Boolean = address.im <= -2

  private case class State(grid: Grid[(Char, Complex[Int])], movementCost: Int) {
    override def hashCode(): Int = grid.hashCode()

    override def equals(obj: Any): Boolean = obj match {
      case State(g, _) => grid == g
      case _ => false
    }
  }
}

case object Day23 extends App {
  val input = Files.lines("2021/day23.txt")
  val problem = Day23(input)
  Timer.time(() => problem.solve1())
  Timer.time(() => problem.solve2())
}
