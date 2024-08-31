package space.scown.adventofcode
package advent2018.problems

import lib.{Files, Problem, Timer}

import scala.annotation.tailrec

case class Day9(input: String) extends Problem {
  override def solve1(): Unit = {
    val (players, lastMarble) = parse()

    val result = solve(players, lastMarble)

    println(s"Result 1: $result")
  }


  override def solve2(): Unit = {
    val (players, initialLastMarble) = parse()
    val lastMarble = initialLastMarble * 100

    val result = solve(players, lastMarble)

    println(s"Result 2: $result")
  }

  private def solve(players: Int, lastMarble: Int): Long = {
    @tailrec
    def helper(front: Vector[Int], back: Vector[Int], newMarble: Int, currentPlayer: Int, scores: Map[Int, Long]): Long = {
      if (newMarble > lastMarble) scores.maxBy(_._2)._2
      else {
        if (newMarble % 23 == 0) {
          val pointsScored = newMarble + front.head
          val newCurrentMarble = front.tail.head
          val updatedScores = scores + (currentPlayer -> (scores.getOrElse(currentPlayer, 0L) + pointsScored))
          val (frontOfBack, backOfBack) = back.splitAt(back.size - 7)

          helper(backOfBack :+ newCurrentMarble, front.tail.tail ++ frontOfBack, newMarble + 1, (currentPlayer + 1) % players, updatedScores)
        }
        else {
          val (actualFront, actualBack) = if (back.isEmpty) (back, front) else (front, back)
          val (frontAfterMove, backAfterMove) = (actualFront :+ actualBack.head :+ newMarble, actualBack.tail)
          val (frontAfterCycle, backAfterCycle) = if (frontAfterMove.size > 8) {
            (frontAfterMove.tail.tail, backAfterMove :+ frontAfterMove.head :+ frontAfterMove.tail.head)
          } else {
            (frontAfterMove, backAfterMove)
          }

          helper(frontAfterCycle, backAfterCycle, newMarble + 1, (currentPlayer + 1) % players, scores)
        }
      }
    }

    helper(Vector(0), Vector(), 1, 0, Map())
  }

  private def parse(): (Int, Int) = {
    val pattern = "(\\d+) players; last marble is worth (\\d+) points".r

    input match {
      case pattern(players, lastMarble) => (players.toInt, lastMarble.toInt)
    }
  }
}

case object Day9 extends App {
  val input = Files.lines("2018/day9.txt").head
  val problem = Day9(input)
  problem.solve1()
  Timer.time(() => problem.solve2())
}
