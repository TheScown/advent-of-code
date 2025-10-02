package space.scown.adventofcode
package advent2020

import lib.{Files, Problem}

import scala.annotation.tailrec

case class Day19(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val (baseRules, compoundRules, strings) = parse()

    val patternString = buildPattern(baseRules, compoundRules)
    val pattern = patternString.r

    val matchingStrings = strings.filter(pattern.matches)
    val result = matchingStrings.size

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val (baseRules, compoundRules, strings) = parse()

    @tailrec
    def helper(history: Vector[Int]): Int = {
      val patternString = buildPattern2(baseRules, compoundRules, history.size + 1)
      val pattern = patternString.r

      val matchingStrings = strings.filter(pattern.matches)
      val result = matchingStrings.size

      if (history.nonEmpty && result == history.last) result
      else helper(history :+ result)
    }

    val result = helper(Vector())

    println(s"Result 2: $result")
  }

  private def buildPattern(baseRules: Map[Int, String], compoundRules: Map[Int, Vector[Vector[Int]]]): String = {
    val prerequisites = compoundRules.map { case (id, rules) =>
      (id, rules.flatten.toSet)
    }

    @tailrec
    def helper(found: Map[Int, String], toFind: Set[Int]): Map[Int, String] = {
      if (found.contains(0)) found
      else {
        val canFind = toFind.filter { id =>
          prerequisites(id).forall(found.contains)
        }

        val newRules = canFind.toVector.map { id =>
          val ruleParts = compoundRules(id)

          val newRule = ruleParts.map { ids =>
            ids.map(found).mkString("")
          }.mkString("|")

          (id, s"(?:$newRule)")
        }.toMap

        helper(found ++ newRules, toFind diff canFind)
      }
    }

    val basicPattern = helper(baseRules, compoundRules.keySet)(0)
    s"^$basicPattern$$"
  }

  private def buildPattern2(
    baseRules: Map[Int, String],
    compoundRules: Map[Int, Vector[Vector[Int]]],
    rule11Attempts: Int
  ): String = {
    val prerequisites = compoundRules.map { case (id, rules) =>
      (id, rules.flatten.toSet)
    }

    @tailrec
    def helper(found: Map[Int, String], toFind: Set[Int]): Map[Int, String] = {
      if (found.contains(0)) found
      else {
        val canFind = toFind.filter { id =>
          prerequisites(id).forall(found.contains)
        }

        val newRules = canFind.toVector.map { id =>
          val ruleParts = compoundRules(id)

          val newRule = if (id == 8) found(ruleParts.head.head) + "+"
          else if (id == 11) {
            (1 to rule11Attempts).map(i => found(ruleParts.head.head) * i + found(ruleParts.head.last) * i).mkString("|")
          }
          else ruleParts.map { ids =>
            ids.map(found).mkString("")
          }.mkString("|")

          (id, s"(?:$newRule)")
        }.toMap

        helper(found ++ newRules, toFind diff canFind)
      }
    }

    val basicPattern = helper(baseRules, compoundRules.keySet)(0)
    s"^$basicPattern$$"
  }

  private def parse(): (Map[Int, String], Map[Int, Vector[Vector[Int]]], Vector[String]) = {
    val (rules, strings) = input.span(_.nonEmpty)
    val idPattern = "(\\d+): (.*?)".r

    val idsAndRules = rules.map {
      case idPattern(id, rest) => (id.toInt, rest)
    }

    val (baseRules, otherRules) = idsAndRules.partition { case (_, rule) =>
      rule.contains("\"")
    }

    val cleanBaseRules = baseRules.map { case (id, string) =>
      (id, string.replace("\"", ""))
    }

    val compoundRules = otherRules.map { case (id, rule) =>
      (id, rule.split("\\|").map(s => s.trim.split("\\s+").map(_.toInt).toVector).toVector)
    }.toMap

    (cleanBaseRules.toMap, compoundRules, strings.tail)
  }
}

case object Day19 extends App {
  val input = Files.lines("2020/day19.txt")
  val problem = Day19(input)
  problem.solve1()
  problem.solve2()
}
