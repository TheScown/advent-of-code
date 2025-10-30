package space.scown.adventofcode
package advent2020

import lib.{Files, Problem}

import scala.annotation.tailrec

case class Day22(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val (player1, player2) = parse()

    val winningHand = play(player1, player2)

    val result = score(winningHand)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val (player1, player2) = parse()

    val (_, winningHand) = recursiveCombat(player1, player2)

    val result = score(winningHand)

    println(s"Result 2: $result")
  }

  private def score(winningHand: Vector[Int]): Int = {
    winningHand.zipWithIndex.map { case (c, i) => c * (winningHand.size - i) }.sum
  }

  @tailrec
  private def play(player1: Vector[Int], player2: Vector[Int]): Vector[Int] = {
    if (player1.isEmpty) player2
    else if (player2.isEmpty) player1
    else {
      val player1Card = player1.head
      val player2Card = player2.head

      if (player1Card > player2Card) play(player1.tail :+ player1Card :+ player2Card, player2.tail)
      else play(player1.tail, player2.tail :+ player2Card :+ player1Card)
    }
  }

  private def recursiveCombat(player1: Vector[Int], player2: Vector[Int]): (Player, Vector[Int]) = {
    @tailrec
    def round(player1: Vector[Int], player2: Vector[Int], seen: Set[(Vector[Int], Vector[Int])]): (Player, Vector[Int]) = {
      if (player1.isEmpty) (Player2, player2)
      else if (player2.isEmpty) (Player1, player1)
      else if (seen.contains((player1, player2))) (Player1, player1)
      else {
        val player1Card = player1.head
        val player2Card = player2.head

        val winner = if (player1.tail.size >= player1Card && player2.tail.size >= player2Card) {
          val (winner, _) = recursiveCombat(player1.tail.take(player1Card), player2.tail.take(player2Card))

          winner
        }
        else if (player1Card > player2Card) Player1
        else Player2

        val updatedSeen = seen + ((player1, player2))

        if (winner == Player1) round(player1.tail :+ player1Card :+ player2Card, player2.tail, updatedSeen)
        else round(player1.tail, player2.tail :+ player2Card :+ player1Card, updatedSeen)
      }
    }

    round(player1, player2, Set())
  }

  private def parse(): (Vector[Int], Vector[Int]) = {
    val (player1, rest) = input.span(_.nonEmpty)
    val player2 = rest.tail

    (player1.tail.map(_.toInt), player2.tail.map(_.toInt))
  }

  private sealed trait Player
  private case object Player1 extends Player
  private case object Player2 extends Player
}

case object Day22 extends App {
  val input = Files.lines("2020/day22.txt")
  val problem = Day22(input)
  problem.solve1()
  problem.solve2()
}