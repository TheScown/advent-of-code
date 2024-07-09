package space.scown.adventofcode
package advent2016.problems

import lib.{Files, Problem}

case class Day4(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val entries = parse()

    val result = entries.filter(_.isReal).map(_.sectorId).sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val entries = parse()

    val result = entries.find(e => e.decoded == "northpole object storage").map(_.sectorId)

    println(s"Result 2: $result")
  }

  private def parse(): Vector[Entry] = {
    val pattern = "((?:[a-z]+-?)+)-(\\d+)\\[([a-z]{5})]".r

    input.map { line =>
      val groups = pattern.findFirstMatchIn(line).get
      val name = groups.group(1)
      val sectorId = groups.group(2).toInt
      val checksum = groups.group(3)

      Entry(sectorId, name, checksum)
    }
  }

  private case class Entry(sectorId: Int, characters: String, checksum: String) {
    lazy val isReal: Boolean = {
      val sortedCounts = characters.replace("-", "").toVector.groupBy(identity).view.mapValues(_.size).toVector.sorted {
        (a: (Char, Int), b: (Char, Int)) => {
          val (aChar, aCount) = a
          val (bChar, bCount) = b
          val compareCount = bCount.compareTo(aCount)

          if (compareCount != 0) compareCount else aChar.compareTo(bChar)
        }
      }

      val top5 = sortedCounts.take(5)

      top5.map(_._1).zip(checksum.toVector).forall(p => p._1 == p._2)
    }

    lazy val decoded: String = {
      characters.map {
        case '-' => ' '
        case c =>
          (((c - 'a') + sectorId) % 26 + 'a').toChar
      }
    }
  }
}

case object Day4 extends App {
  val input = Files.lines("2016/day4.txt")
  val problem = Day4(input)
  problem.solve1()
  problem.solve2()
}
