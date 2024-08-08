package space.scown.adventofcode
package advent2017.knothash

import scala.annotation.tailrec

case object KnotHash {

  def knotHash(string: String): String = {
    knotHash(string.toList.map(_.toInt) ++ List(17, 31, 73, 47, 23))
  }

  private def knotHash(input: List[Int]): String = {
    val finalValues = (0 until 64).foldLeft(HashState((0 to 255).toVector, 0, 0, input)) { (state, _) =>
      knotHashRound(state.copy(lengths = input))
    }.values

    finalValues.grouped(16).map(_.reduce(_ ^ _)).map("%02x".format(_)).mkString("")
  }

  @tailrec
  def knotHashRound(state: HashState): HashState = state match {
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

  case class HashState(values: Vector[Int], index: Int, skipSize: Int, lengths: List[Int])

}
