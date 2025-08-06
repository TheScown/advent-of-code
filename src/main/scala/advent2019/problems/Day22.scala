package space.scown.adventofcode
package advent2019.problems

import lib.{Files, Integers, Problem}

import scala.annotation.tailrec

case class Day22(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val numberOfCards = 10007
    val instructions = parse()

    @tailrec
    def helper(cards: Vector[Int], remainingInstructions: Vector[Instruction]): Vector[Int] = {
      if (remainingInstructions.isEmpty) cards
      else {
        val newDeck = remainingInstructions.head match {
          case Reverse => cards.reverse
          case Cut(x) =>
            val (start, end) = if (x >= 0) cards.splitAt(x)
            else cards.splitAt(cards.size + x)

            end ++ start
          case Increment(increment) =>
            val newIndexes = cards.zipWithIndex.map { case (_, i) => (i * increment) % cards.size }
            newIndexes.zipWithIndex.foldLeft(cards) { case (newCards, (ni, i)) =>
              newCards.updated(ni, cards(i))
            }
        }

        helper(newDeck, remainingInstructions.tail)
      }
    }

    val initialDeck = (0 until numberOfCards).toVector
    val finalDeck = helper(initialDeck, instructions)
    val result = finalDeck.indexOf(2019)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val numberOfCards = BigInt(119315717514047L)
    val numberOfShuffles = BigInt(101741582076661L)
    val instructions = parse()

    // See https://codeforces.com/blog/entry/72593
    @tailrec
    def shuffleCoefficients(a: BigInt, b: BigInt, remainingInstructions: Vector[Instruction]): (BigInt, BigInt) = {
      if (remainingInstructions.isEmpty) (a, b)
      else {
        val (newA, newB) = remainingInstructions.head match {
          case Reverse => ((-1 * a) % numberOfCards, (-1 * b + -1) % numberOfCards)
          case Cut(x) => ((1 * a) % numberOfCards, (1 * b + -x) % numberOfCards)
          case Increment(increment) => ((increment * a) % numberOfCards, (increment * b) % numberOfCards)
        }

        shuffleCoefficients(newA, newB, remainingInstructions.tail)
      }
    }

    val x0 = BigInt(2020)
    val (a, b) = shuffleCoefficients(1, 0, instructions)
    val aMinus1Inverse = (a - 1).modPow(numberOfCards - 2, numberOfCards)

    val finalA = a.modPow(numberOfShuffles, numberOfCards)
    // Via Geometric series
    val finalB = b * (a.modPow(numberOfShuffles, numberOfCards) - 1) * aMinus1Inverse

    // Via Fermat's little theorem as number of cards is prime
    val finalAInverse = finalA.modPow(numberOfCards - 2, numberOfCards)
    val possibleResult = ((x0 - finalB) * finalAInverse) % numberOfCards
    val result = if (possibleResult < 0) possibleResult + numberOfCards else possibleResult

    println(s"Result 2: $result")
  }

  private def parse(): Vector[Instruction] = {
    val reverse = "deal into new stack"
    val cut = "cut (-?\\d+)".r
    val increment = "deal with increment (\\d+)".r

    input.map {
      case l if l == reverse => Reverse
      case cut(x) => Cut(x.toInt)
      case increment(x) => Increment(x.toInt)
    }
  }

  private sealed trait Instruction
  private case object Reverse extends Instruction
  private case class Cut(by: Int) extends Instruction
  private case class Increment(increment: Int) extends Instruction
}

case object Day22 extends App {
  val input = Files.lines("2019/day22.txt")
  val problem = Day22(input)
  problem.solve1()
  problem.solve2()
}
