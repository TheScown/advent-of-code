package space.scown.adventofcode
package advent2017.problems

import lib.{Files, Problem}

import scala.annotation.tailrec

case class Day10(input: String) extends Problem {
  override def solve1(): Unit = {
    val lengths = if (input.isEmpty) Nil else input.split(",").map(_.toInt).toList

    val finalList = knotHashRound(HashState((0 to 255).toVector, 0, 0, lengths)).values
    val result = finalList(0) * finalList(1)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val lengths = input.toList.map(_.toInt) ++ List(17, 31, 73, 47, 23)

    val result = knotHash(lengths)

    println(s"Result 2: $result")
  }

  private def knotHash(input: List[Int]): String = {
    val finalValues = (0 until 64).foldLeft(HashState((0 to 255).toVector, 0, 0, input)) { (state, _) =>
      knotHashRound(state.copy(lengths = input))
    }.values

    finalValues.grouped(16).map(_.reduce(_ ^ _)).map("%02x".format(_)).mkString("")
  }

  @tailrec
  private def knotHashRound(state: HashState): HashState = state match {
    case state@HashState(values, index, skipSize, lengths) =>
      if (lengths.isEmpty) state
      else {
        val nextLength = lengths.head

        if (index + nextLength > values.size) {
          val (front, toEnd) = values.splitAt(index)
          val (fromFront, tail) = front.splitAt(index + nextLength - values.size)
          val toEndSize = toEnd.size
          val reversed = (toEnd ++ fromFront).reverse
          val (newToEnd, newFromFront) = reversed.splitAt(toEndSize)

          knotHashRound(HashState(newFromFront ++ tail ++ newToEnd, (index + nextLength + skipSize) % values.size, skipSize + 1, lengths.tail))
        }
        else {
          val (before, rest) = values.splitAt(index % values.length)
          val (toReverse, after) = rest.splitAt(nextLength)

          knotHashRound(HashState(before ++ toReverse.reverse ++ after, (index + nextLength + skipSize) % values.size, skipSize + 1, lengths.tail))
        }
      }
  }

  private case class HashState(values: Vector[Int], index: Int, skipSize: Int, lengths: List[Int])
}

case object Day10 extends App {
  val input = Files.lines("2017/day10.txt").head
  val problem = Day10(input)
  problem.solve1()
  problem.solve2()
}
