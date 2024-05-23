package space.scown.adventofcode
package advent2015.problems.day7

import lib.Timer.time
import lib.{Files, Problem}

import scala.util.parsing.input.PagedSeqReader

case class Day7(operations: Map[String, Operation]) extends Problem {
  override def solve1(): Unit = {
    val result = getResult("a", operations)

    println(s"Result 1: ${result.toInt}")
  }

  override def solve2(): Unit = {
    val firstResult = getResult("a", operations)

    val updatedOperations = operations + ("b" -> Fixed(firstResult))

    val finalResult = getResult("a", updatedOperations)

    println(s"Result 2: ${finalResult.toInt}")
  }

  private def getResult(name: String, operations: Map[String, Operation]): Char = {
    def helper(name: String, cache: Map[String, Char]): (Char, Map[String, Char]) = {
      cache.get(name) match {
        case Some(c) =>
          (c, cache)
        case None =>
          val operation = operations(name)
          val (result, updatedCache) = operation match {
            case Fixed(c) => (c, cache)
            case Passthrough(name) => helper(name, cache)
            case Not(name) =>
              val (value, updatedCache) = helper(name, cache)
              ((~value).toChar, updatedCache)
            case And(left, right) =>
              val (l, cache1) = helper(left, cache)
              val (r, cache2) = helper(right, cache1)
              ((l & r).toChar, cache2)
            case FixedAnd(left, right) =>
              val (r, cache2) = helper(right, cache)
              ((left & r).toChar, cache2)
            case Or(left, right) =>
              val (l, cache1) = helper(left, cache)
              val (r, cache2) = helper(right, cache1)
              ((l | r).toChar, cache2)
            case LShift(left, right) =>
              val (l, updatedCache) = helper(left, cache)
              ((l << right).toChar, updatedCache)
            case RShift(left, right) =>
              val (l, updatedCache) = helper(left, cache)
              ((l >> right).toChar, updatedCache)
          }

          (result, updatedCache + (name -> result))
      }
    }

    helper(name, Map())._1
  }
}

case object Day7 extends App {
  val reader = new PagedSeqReader(Files.pagedSequence("2015/day7.txt"))
  Grammar.parseAll[Map[String, Operation]](Grammar.operations, reader) match {
    case Grammar.Success(operations, _) =>
      val day = Day7(operations)
      time(() => day.solve1())
      time(() => day.solve2())
    case error@Grammar.Failure(_, _) => throw new IllegalArgumentException(s"$error")
    case Grammar.Error(msg, _) => throw new IllegalArgumentException(msg)
  }
}
