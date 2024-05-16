package space.scown.adventofcode
package advent2023.problems

import lib.{Files, Problem}

import scala.annotation.tailrec

case class Day4(lines: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val result = lines.map { line =>
      val colonSplit = line.split(":")
      val setsString = colonSplit(1)
      val setStrings = setsString.split("\\|")
      val sets = setStrings.map(makeSet)
      val intersection = sets(0).intersect(sets(1))

      if (intersection.isEmpty) 0
      else Math.pow(2, intersection.size - 1).toInt
    }.sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val cards = lines.map { line =>
      val colonSplit = line.split(":")
      val id = colonSplit(0).replaceAll("Card\\s+", "").toInt
      val setsString = colonSplit(1)
      val setStrings = setsString.split("\\|")
      val sets = setStrings.map(makeSet)
      val intersection = sets(0).intersect(sets(1))

      Card(id, intersection.size)
    }.toList

    @tailrec
    def helper(cards: Seq[Card], count: Int, futureCards: Map[Int, Int]): Int = {
      if (cards.isEmpty) {
        return count
      }

      cards.head match {
        case Card(id, intersection) =>
          val currentCardCount = futureCards.getOrElse(id, 1)

          val mapUpdates = Map.from((1 to intersection).map { i =>
            id + i -> (futureCards.getOrElse(id + i, 1) + currentCardCount)
          })

          helper(cards.tail, count + currentCardCount, futureCards ++ mapUpdates)
      }
    }

    val result = helper(cards, 0, Map())

    println(s"Result 2: $result")
  }

  private def makeSet(input: String): Set[Int] = {
    Set.from(input.trim().split(" ").map(_.trim).filterNot(_.isEmpty).map(_.toInt))
  }
}

case class Card(id: Int, intersection: Int)

object Day4 {
  def main(args: Array[String]): Unit = {
    val value = Files.lines("2023/day4.txt")
    Day4(value).solve1()
    Day4(value).solve2()
  }

}
