package space.scown.adventofcode
package advent2024.problems

import lib.{Files, Problem, Timer}

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

    val prices = allSecretNumbers.map(sns => sns.map(_ % 10).map(_.toInt))

    val deltas = prices.map(priceRow => priceRow.zip(priceRow.tail).map {
      case (x0, x1) => x1 - x0
    })

    val sequenceMaps = deltas.map { deltaRow =>
      val sliding = deltaRow.sliding(4)

      sliding.zipWithIndex.foldLeft(Map[Vector[Int], Int]()) { case (map, (sequence, index)) =>
        if (map.contains(sequence)) map
        else map + (sequence -> index)
      }
    }

    val pricesWithSequenceIndexes = prices.zip(sequenceMaps)

    val allSequences = deltas.flatMap(_.sliding(4))

    val sequenceCounts = allSequences.groupBy(identity).map { case (k, v) => (k, v.size) }

    val sortedSequenceCounts = sequenceCounts.toVector.sortBy(_._2).reverse

    val result = sortedSequenceCounts.map(_._1).take(sortedSequenceCounts.size / 100).map { sequence =>
      pricesWithSequenceIndexes.map { case (priceRow, sequenceIndex) =>
        sequenceIndex.get(sequence) match {
          case Some(i) => priceRow(i + 4)
          case None => 0
        }
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
  Timer.time(() => problem.solve2())
}
