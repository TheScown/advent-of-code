package space.scown.adventofcode
package advent2023.problems.day3

import lib.{Files, Problem}

import scala.collection.immutable.SortedMap
import scala.util.parsing.input.PagedSeqReader

case class Day3(input: Seq[Token]) extends Problem {
  override def solve1(): Unit = {
    val (numbers: Seq[Number], symbols) = input.partition {
      case Number(_) => true
      case _ => false
    }

    val numberIndex = SortedMap.from(numbers.flatMap {
      case token@Number(value) => (0 until value.length).map(i => Address(token.pos.line, token.pos.column + i) -> token)
    })

    val usedNumberTokens = symbols.flatMap { token =>
      val line = token.pos.line
      val column = token.pos.column

      Set.from(
        (-1 to 1).flatMap(i => {
          (-1 to 1).map(j => {
            val address = Address(line + i, column + j)
            numberIndex.getOrElse(address, Number("0"))
          })
        })
      )
    }

    val result = usedNumberTokens.map {
      case Number(value) => value.toInt
    }.sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val (numbers: Seq[Number], symbols) = input.partition {
      case Number(_) => true
      case _ => false
    }

    val stars: Seq[Star] = symbols.filter {
      case Star() => true
      case _ => false
    }.asInstanceOf[Seq[Star]]

    val numberIndex = SortedMap.from(numbers.flatMap {
      case token @ Number(value) => (0 until value.length).map(i => Address(token.pos.line, token.pos.column + i) -> token)
    })

    val gearRatios = stars.map { token =>
      val line = token.pos.line
      val column = token.pos.column

      val matchingNumbers = Set.from(
        (-1 to 1).flatMap(i => {
          (-1 to 1).map(j => {
            val address = Address(line + i, column + j)
            numberIndex.getOrElse(address, Number("0"))
          })
        })
      ).filterNot {
        case Number(value) => value == "0"
      }

      if (matchingNumbers.size == 2) {
        matchingNumbers.map {
          case Number(value) => value.toInt
        }.product
      } else {
        0
      }
    }

    val result = gearRatios.sum

    println(s"Result 2: $result")
  }
}

case class Address(line: Int, column: Int) extends Ordered[Address] {

  override def compare(that: Address): Int = {
    val lineDiff = line - that.line

    if (lineDiff != 0) lineDiff
    else column - that.column
  }

}

object Day3 {
  def main(args: Array[String]): Unit = {
    val reader = new PagedSeqReader(Files.pagedSequence("2023/day3.txt"))
    Grammar.parseAll[Seq[Token]](Grammar.tokens, reader) match {
      case Grammar.Success(tokens, _) =>
        val day = Day3(tokens.filterNot(t => t == Dot()))
        day.solve1()
        day.solve2()
      case Grammar.Failure(msg, _) => throw new IllegalArgumentException(msg)
      case Grammar.Error(msg, _) => throw new IllegalArgumentException(msg)
    }
  }
}