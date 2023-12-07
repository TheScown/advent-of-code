package space.scown.advent2023
package problems.day7

import lib.{Files, Problem}

case class Day7(lines: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val sortedRounds = lines.map { line =>
      val parts = line.split(" ")
      Round(new Hand(parts(0)), parts(1).toLong)
    }.sorted

    val zipped = (1 to sortedRounds.size).zip(sortedRounds)
    val result = zipped.map {
      case (i, Round(_, bid)) => bid * i
    }.sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val rounds = lines.map { line =>
      val parts = line.split(" ")
      Round(new Hand(parts(0)), parts(1).toLong)
    }

    val sortedRounds = rounds
      .sorted { (x: Round, y: Round) =>
        val h1 = x.hand
        val h2 = y.hand
        val kindCompare = h1.kind2.compare(h2.kind2)

        if (kindCompare != 0) kindCompare
        else {
          h1.cards.zip(h2.cards).find { (p: (Card, Card)) =>
            p match {
              case (c1, c2) => c1.rank2.compare(c2.rank2) != 0
            }
          } match {
            case Some((c1, c2)) => c1.rank2.compare(c2.rank2)
            case None => 0
          }
        }
      }

    val zipped = (1 to sortedRounds.size).zip(sortedRounds)
    val result = zipped.map {
      case (i, Round(_, bid)) => bid * i
    }.sum

    println(s"Result 2: $result")
  }
}

case class Round(hand: Hand, bid: Long) extends Ordered[Round] {
  override def compare(that: Round): Int = hand.compare(that.hand)
}

case class Hand(cards: Vector[Card]) extends Ordered[Hand] {

  def this(input: String) = {
    this(
      input.split("").map(_.charAt(0)).map {
        case 'A' => Ace
        case 'K' => King
        case 'Q' => Queen
        case 'J' => Jack
        case 'T' => Ten
        case c => Number(c.asDigit)
      }.toVector
    )
  }

  val kind1: Kind = {
    val index = cards.groupBy(c => c.rank1)

    if (index.size == 1) FiveKind
    else if (index.size == 2) {
      val sizes = index.values.map(v => v.size).toSet

      if (sizes.contains(4)) FourKind
      else FullHouse
    }
    else if (index.size == 3) {
      val sizes = index.values.map(v => v.size).toVector

      if (sizes.contains(3)) ThreeKind
      else TwoPair
    }
    else if (index.size == 4) Pair
    else HighCard
  }

  val kind2: Kind = {
    val index = cards.groupBy(c => c.rank2)
    val jokerCount = index.getOrElse(1, Vector.empty).size

    val kind = if (index.size == 1) FiveKind
    else if (index.size == 2) {
      val sizes = index.values.map(v => v.size).toSet

      if (sizes.contains(4)) {
        if (jokerCount == 1 || jokerCount == 4) FiveKind
        else FourKind
      }
      else {
        if (jokerCount == 3 || jokerCount == 2) FiveKind
        else FullHouse
      }
    }
    else if (index.size == 3) {
      val sizes = index.values.map(v => v.size).toVector

      if (sizes.contains(3)) {
        if (jokerCount == 1 || jokerCount == 3) FourKind
        else ThreeKind
      }
      else {
        if (jokerCount == 2) FourKind
        else if (jokerCount == 1) FullHouse
        else TwoPair
      }
    }
    else if (index.size == 4) {
      if (jokerCount == 2 || jokerCount == 1) ThreeKind
      else Pair
    }
    else {
      if (jokerCount == 1) Pair
      else HighCard
    }

    kind
  }

  override def compare(that: Hand): Int = {
    val kindCompare = kind1.compare(that.kind1)

    if (kindCompare != 0) kindCompare
    else {
      cards.zip(that.cards).find { (p: (Card, Card)) => p match {
        case (c1, c2) => c1.compare(c2) != 0
      } } match {
        case Some((c1, c2)) => c1.compare(c2)
        case None => 0
      }
    }
  }
}

object Day7 {
  def main(args: Array[String]): Unit = {
    val value = Files.lines("day7.txt")
    Day7(value).solve1()
    Day7(value).solve2()
  }

}
