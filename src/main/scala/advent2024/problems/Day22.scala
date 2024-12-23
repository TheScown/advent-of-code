package space.scown.adventofcode
package advent2024.problems

import lib.{Files, Problem}

case class Day22(input: Vector[String]) extends Problem {
  private val PRUNING_CONSTANT = 16777216L

  override def solve1(): Unit = {
    val result = input.map { line =>
      val initialSecretNumber = line.toLong

      (1 to 2000).foldLeft(initialSecretNumber) {(secretNumber, _) => nextSecretNumber(secretNumber)}
    }.sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val allSecretNumbers = input.map { line =>
      val initialSecretNumber = line.toLong

      (1 to 2000).foldLeft(Vector(initialSecretNumber)) {(acc, _) => acc :+ nextSecretNumber(acc.last)}
    }

    val prices = allSecretNumbers.map(sns => sns.map(_ % 10))

    val deltas = prices.map(priceRow => priceRow.zip(priceRow.tail).map {
      case (x0, x1) => x1 - x0
    })

    val pricesWithDeltas = prices.zip(deltas)

    val allSequences = for {
      i <- -9 to 9
      j <- -9  to 9
      k <- -9 to 9
      l <- -9 to 9
    } yield Seq(i, j, k, l)

    val result = allSequences.map { sequence =>
      pricesWithDeltas.map { case (priceRow, deltaRow) =>
        val indexOfSequence = deltaRow.sliding(4).indexOf(sequence)

        if (indexOfSequence == -1) 0
        else priceRow(indexOfSequence + 4)
      }.sum
    }.max

    println(s"Result 2: $result")
  }

  private def nextSecretNumber(initialSecretNumber: Long) = {
    val step1 = ((initialSecretNumber * 64) ^ initialSecretNumber) % PRUNING_CONSTANT
    val step2 = ((step1 / 32) ^ step1) % PRUNING_CONSTANT
    val step3 = ((step2 * 2048) ^ step2) % PRUNING_CONSTANT
    step3
  }
}

case object Day22 extends App {
  val input = Files.lines("2024/day22.txt")
  val problem = Day22(input)
  problem.solve1()
  problem.solve2()
}
