package space.scown.adventofcode
package advent2021

import lib.{Files, Problem}

import scala.annotation.tailrec

case class Day21(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val (player1Start, player2Start) = parse()

    // Determined by inspection
    val player1Cycle = Vector(6, 0, 2, 2, 0, 6, 0, 2, 2, 0).map(scoreCycle(player1Start))
    val player2Cycle = Vector(5, 8, 9, 8, 5, 0, 3, 4, 3, 0).map(scoreCycle(player2Start))

    val player1Needs = turnsToGivenScore(player1Cycle, 1000)
    val player2Needs = turnsToGivenScore(player2Cycle, 1000)

    val (winningTurns, losingCycle, unequalTurns) = if (player1Needs <= player2Needs) (player1Needs, player2Cycle, true) else (player2Needs, player1Cycle, false)
    val losingScore = scoreAfterTurns(losingCycle, if (unequalTurns) winningTurns - 1 else winningTurns)
    val diceRolls = 3 * 2 * winningTurns - (if (unequalTurns) 3 else 0)

    val result = losingScore * diceRolls
    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val (player1Start, player2Start) = parse()

    val possibleRolls = for {
      i <- 1 to 3
      j <- 1 to 3
      k <- 1 to 3
    } yield Vector(i, j, k)

    val distribution = possibleRolls.groupBy(_.sum).map {
      case (k, v) => k -> v.size
    }.toVector

    val initialStates = Map(State(
      player1Start, 0,
      player2Start, 0,
      player1Turn = true
    ) -> 1L)

    @tailrec
    def helper(states: Map[State, Long]): Map[State, Long] = {
      val (terminal, nonTerminal) = states.partition { case (state, _) =>
        state.p1Score >= 21 || state.p2Score >= 21
      }

      if (nonTerminal.isEmpty) terminal
      else {
        val newStateMap = nonTerminal.foldLeft(Map[State, Long]()) { case (acc, (state, count)) =>
          val startPosition = if (state.player1Turn) state.p1Position else state.p2Position
          val startScore = if (state.player1Turn) state.p1Score else state.p2Score

          val newStates = distribution.map { case (roll, rollCount) =>
            val newPosition = (roll + startPosition) % 10
            val newScore = (if (newPosition == 0) 10 else newPosition) + startScore
            val newCount = count * rollCount

            val newState = if (state.player1Turn) state.copy(p1Position = newPosition, p1Score = newScore, player1Turn = false)
            else state.copy(p2Position = newPosition, p2Score = newScore, player1Turn = true)

            (newState, newCount)
          }

          newStates.foldLeft(acc) { case (acc, (newState, newCount)) =>
            val currentCount = acc.getOrElse(newState, 0L)
            acc + (newState -> (newCount + currentCount))
          }
        }

        val mergedWithTerminal = newStateMap.foldLeft(terminal) { case (acc, (newState, newCount)) =>
          val currentCount = acc.getOrElse(newState, 0L)
          acc + (newState -> (newCount + currentCount))
        }

        helper(mergedWithTerminal)
      }
    }

    val winningStates = helper(initialStates)
    val (p1Wins, p2Wins) = winningStates.partition { case (state, count) =>
      state.p1Score > state.p2Score
    }

    val p1Sum = p1Wins.values.sum
    val p2Sum = p2Wins.values.sum
    val result = math.max(p1Sum, p2Sum)

    println(s"Result 2: $result")
  }

  private def turnsToGivenScore(cycle: Vector[Int], score: Int): Int = {
    val cycleScore = cycle.sum
    val fullCycles = score / cycleScore
    val scoreAfterFullCycles = cycleScore * fullCycles
    val remainingTurns = (1 until cycle.size).find(i => cycle.take(i).sum + scoreAfterFullCycles >= score).get
    fullCycles * cycle.length + remainingTurns
  }

  private def scoreAfterTurns(cycle: Vector[Int], turns: Int): Int = {
    val cycleScore = cycle.sum
    val fullCycles = turns / cycle.size
    val partialCycle = cycle.take(turns % cycle.size)
    cycleScore * fullCycles + partialCycle.sum
  }

  private def scoreCycle(playerStart: Int)(space: Int) = {
    val finalSpace = (space + playerStart) % 10
    if (finalSpace == 0) 10 else finalSpace
  }

  private def parse(): (Int, Int) = {
    (input.head.last.asDigit, input.last.last.asDigit)
  }

  case class State(p1Position: Int, p1Score: Int, p2Position: Int, p2Score: Int, player1Turn: Boolean)
}

case object Day21 extends App {
  val input = Files.lines("2021/day21.txt")
  val problem = Day21(input)
  problem.solve1()
  problem.solve2()
}
