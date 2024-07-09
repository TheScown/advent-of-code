package space.scown.adventofcode
package advent2016.problems

import lib.{Files, Problem}

import scala.annotation.tailrec

case class Day7(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val pattern = "(.)(.)\\2\\1".r

    val result = input.count { line =>
      val parts = line.split("[\\[\\]]").zipWithIndex
      val (nonHyper, hyper) = parts.partition(_._2 % 2 == 0)

      val hasValidSequence = nonHyper.map(_._1).exists(s => pattern.findFirstIn(s) match {
        case Some(abba) => abba(0) != abba(1)
        case None => false
      })

      val hasNoInvalidHypernetSequences = hyper.map(_._1).forall(s => pattern.findFirstIn(s) match {
        case Some(abba) => abba(0) == abba(1)
        case None => true
      })

      hasValidSequence && hasNoInvalidHypernetSequences
    }

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val pattern = "(.).\\1".r

    val result = input.count { line =>
      val parts = line.split("[\\[\\]]").zipWithIndex
      val (nonHyper, hyper) = parts.partition(_._2 % 2 == 0)

      val abas = nonHyper.map(_._1).flatMap { nh =>
        @tailrec
        def helper(index: Int = 0, acc: Set[String] = Set()): Set[String] = {
          val stringToTest = nh.substring(index)
          pattern.findFirstMatchIn(stringToTest) match {
            case Some(nextMatch) =>
              helper(index + nextMatch.start + 1, acc + nextMatch.group(0))
            case None => acc
          }
        }

        helper()
      }.toSet

      val hasAba = abas.exists { aba =>
        val bab = s"${aba(1)}${aba(0)}${aba(1)}"
        hyper.map(_._1).exists(h => h.contains(bab))
      }

      hasAba
    }

    println(s"Result 2: $result")
  }
}

case object Day7 extends App {
  val input = Files.lines("2016/day7.txt")
  val problem = Day7(input)
  problem.solve1()
  problem.solve2()
}
