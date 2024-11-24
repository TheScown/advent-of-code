package space.scown.adventofcode
package advent2019.problems

import lib.{Files, Problem}

import scala.annotation.tailrec
import scala.language.postfixOps
import scala.util.parsing.combinator.RegexParsers

case class Day14(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val requirements = parse()

    val (result, _) = requirementsForNFuel(1, Map(), requirements)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val requirements = parse()

    val (requirementsFor1Fuel, _) = requirementsForNFuel(1, Map(), requirements)

    val oreInHold = 1_000_000_000_000L

    @tailrec
    def helper(remainingOre: Long, totalFuel: Long, onHand: Map[String, Long]): Long = {
      val fuelToProduce = remainingOre / requirementsFor1Fuel
      val (requirementsForFuel, newOnHand) = requirementsForNFuel(fuelToProduce, onHand, requirements)

      val nextRemainingOre = remainingOre - requirementsForFuel

      if (nextRemainingOre < requirementsFor1Fuel) {
        // With what's on hand we might be able to make 1 more
        val (requirementsWithOnHand, _) = requirementsForNFuel(1, newOnHand, requirements)

        if (requirementsWithOnHand < nextRemainingOre) totalFuel + fuelToProduce + 1
        else totalFuel + fuelToProduce
      } else helper(nextRemainingOre, totalFuel + fuelToProduce, newOnHand)
    }

    val result = helper(oreInHold, 0, Map())

    println(s"Result 2: $result")
  }

  private def requirementsForNFuel(n: Long, onHand: Map[String, Long], requirements: Map[String, (Set[(String, Long)], Long)]) = {
    def helper(product: String, quantity: Long, onHand: Map[String, Long]): (Long, Map[String, Long]) = {
      val (dependencies, amountProduced) = requirements(product)

      val productionsRequired = Math.ceil(quantity.toDouble / amountProduced.toDouble).toLong
      val surplus = productionsRequired * amountProduced - quantity

      val dependencyTypes = dependencies.map(_._1)

      if (dependencyTypes.contains("ORE")) {
        val (_, dependencyQuantity) = dependencies.head
        (dependencyQuantity * productionsRequired, onHand + (product -> (onHand.getOrElse(product, 0L) + surplus)))
      }
      else {
        val (oreFromDependencies, finalOnHand) = dependencies.foldLeft((0L, onHand)) { case ((oreTotal, onHand), (dependency, quantity)) =>
          val leftovers = onHand.getOrElse(dependency, 0L)
          val totalQuantity = quantity * productionsRequired

          if (leftovers >= totalQuantity) {
            (oreTotal, onHand + (dependency -> (leftovers - totalQuantity)))
          }
          else {
            val quantityToProduce = totalQuantity - leftovers
            val (oreRequired, newOnHand) = helper(dependency, quantityToProduce, onHand + (dependency -> 0L))
            (oreTotal + oreRequired, newOnHand)
          }
        }

        (oreFromDependencies, finalOnHand + (product -> (finalOnHand.getOrElse(product, 0L) + surplus)))
      }
    }

    helper("FUEL", n, onHand)
  }

  private def parse(): Map[String, (Set[(String, Long)], Long)] = {
    Grammar.parseAll[Map[String, (Set[(String, Long)], Long)]](Grammar.requirements, input.mkString("\n")) match {
      case Grammar.Success(result, _) => result
      case Grammar.NoSuccess.I(msg, next) =>
        throw new IllegalArgumentException(s"$msg,$next")
    }
  }

  case object Grammar extends RegexParsers {
    def requirements: Parser[Map[String, (Set[(String, Long)], Long)]] = rep(line) ^^
      (_.toMap)

    def line: Parser[(String, (Set[(String, Long)], Long))] = lhs ~ ("=>" ~> pair) ^^ {
      case lhs ~ pair =>
        pair._1 -> (lhs.toSet, pair._2)
    }

    def lhs: Parser[List[(String, Long)]] = rep1sep(pair, ", ")
    def pair: Parser[(String, Long)] = number ~ name ^^ {
      case number ~ name => (name, number)
    }
    def name: Parser[String] = "[A-Z]+".r
    def number: Parser[Long] = "\\d+".r ^^ (_.toLong)
  }
}

case object Day14 extends App {
  val input = Files.lines("2019/day14.txt")
  val problem = Day14(input)
  problem.solve1()
  problem.solve2()
}
