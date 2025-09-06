package space.scown.adventofcode
package advent2020

import lib.{Files, Problem}

import scala.annotation.tailrec

case class Day16(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val data = parse()

    val rules = data.rules.values

    val result = data.otherTickets.flatMap { ticket =>
      ticket.filter { value =>
        !rules.exists { ranges => ranges.exists(_.contains(value)) }
      }
    }.sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val data = parse()

    val rules = data.rules
    val ruleValues = rules.values

    val validTickets = data.ticket +: data.otherTickets.filter { ticket =>
      ticket.forall { value =>
        ruleValues.exists { ranges => ranges.exists(_.contains(value)) }
      }
    }

    @tailrec
    def helper(knownFields: Map[String, Int], unknownFields: Set[String]): Map[String, Int] = {
      if (unknownFields.isEmpty) knownFields
      else {
        val possibleFields = validTickets.map { ticket =>
          ticket.map { value =>
            rules.filter { case (name, ranges) =>
              unknownFields.contains(name) && ranges.exists(_.contains(value))
            }.keySet
          }
        }

        val intersectedFields = possibleFields.reduce { (a, b) =>
          a.zip(b).map { case (x, y) => x intersect y }
        }.zipWithIndex

        val newlyKnownFields = intersectedFields.filter(_._1.size == 1).map { case (set, index) =>
          (set.head, index)
        }

        helper(knownFields ++ newlyKnownFields, unknownFields diff newlyKnownFields.map(_._1).toSet)
      }
    }

    val fieldMapping = helper(Map(), rules.keySet)
    val requiredRules = rules.filter(_._1.startsWith("departure"))

    val result = requiredRules.keys.map { ruleName =>
      val field = fieldMapping(ruleName)
      data.ticket(field)
    }.product

    println(s"Result 2: $result")
  }

  private def parse(): Data = {
    val (ruleLines, notRules) = input.span(_.nonEmpty)
    val (ticketLines, notTicket) = notRules.tail.span(_.nonEmpty)
    val otherTicketStrings = notTicket.tail.tail

    val ticket = ticketLines.tail.head.split(",").map(_.toLong).toVector
    val otherTickets = otherTicketStrings.map(_.split(",").map(_.toLong).toVector)

    val rulePattern = "([^:]+): (\\d+)-(\\d+) or (\\d+)-(\\d+)".r

    val rules = ruleLines.map {
      case rulePattern(name, start1, end1, start2, end2) =>
        name -> Seq(
          Range.inclusive(start1.toInt, end1.toInt),
          Range.inclusive(start2.toInt, end2.toInt),
        )
    }.toMap

    Data(rules, ticket, otherTickets)
  }

  case class Data(rules: Map[String, Seq[Range]], ticket: Vector[Long], otherTickets: Vector[Vector[Long]])
}

case object Day16 extends App {
  val input = Files.lines("2020/day16.txt")
  val problem = Day16(input)
  problem.solve1()
  problem.solve2()
}
