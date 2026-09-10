package space.scown.adventofcode
package advent2021

import lib.{Files, Problem}

import scala.annotation.tailrec

case class Day14(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val (template, rules) = parse()
    val templateVector = template.toVector

    val finalString = (0 until 10).foldLeft(templateVector) { (template, _) =>
      @tailrec
      def helper(lhs: Vector[Char], rhs: Vector[Char]): Vector[Char] = {
        rhs match {
          case lp +: rest =>
            if (rest.isEmpty) lhs :+ lp
            else {
              val rp = rest.head
              val ruleString = s"$lp$rp"
              val toInsert = rules(ruleString)
              helper(lhs :+ lp :+ toInsert, rest)
            }
        }
      }

      helper(Vector(), template)
    }

    val groupedChars = finalString.groupBy(c => c)

    val charCounts = groupedChars.map { case (k, v) =>
      (k, v.size)
    }

    val max = charCounts.maxBy(_._2)._2
    val min = charCounts.minBy(_._2)._2

    val result = max - min
    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val (template, rules) = parse()
    val startChar = template.head
    val endChar = template.last

    val initialPairCounts = template.sliding(2).foldLeft(Map[String, Long]()) { (counts, pair) =>
      counts + (pair -> (counts.getOrElse(pair, 0L) + 1L))
    }

    val finalPairCounts = (0 until 40).foldLeft(initialPairCounts) { (counts, _) =>
      counts.foldLeft(Map[String, Long]()) { case (map, (pair, value)) =>
        val toInsert = rules(pair)
        Seq(s"${pair.head}$toInsert", s"$toInsert${pair.last}").foldLeft(map) { (map, s) =>
          map + (s -> (map.getOrElse(s, 0L) + value))
        }
      }
    }

    val doubledCharCounts = finalPairCounts.foldLeft(Map[Char, Long]()) { case (charCounts, (pair, count)) =>
      pair.foldLeft(charCounts) { (charCounts, c) =>
        charCounts + (c -> (charCounts.getOrElse(c, 0L) + count))
      }
    }

    val actualCharCounts = doubledCharCounts.map { case (c, v) =>
      if (c == startChar || c == endChar) c -> (((v - 1) / 2) + 1)
      else c -> v / 2
    }

    val max = actualCharCounts.maxBy(_._2)._2
    val min = actualCharCounts.minBy(_._2)._2

    val result = max - min
    println(s"Result 2: $result")
  }

  private def parse(): (String, Map[String, Char]) = {
    val template = input.head
    val ruleLines = input.tail.tail

    val rules = ruleLines.map { line =>
      val parts = line.split(" -> ")
      parts.head -> parts(1).head
    }.toMap

    (template, rules)
  }
}

case object Day14 extends App {
  val input = Files.lines("2021/day14.txt")
  val problem = Day14(input)
  problem.solve1()
  problem.solve2()
}
