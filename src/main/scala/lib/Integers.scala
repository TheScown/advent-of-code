package space.scown.adventofcode
package lib

import scala.annotation.tailrec

object Integers {

  def naturalNumbers: LazyList[Int] = {
    def next(n: Int): LazyList[Int] = {
      n #:: next(n + 1)
    }

    next(1)
  }

  def factors(x: Int): Set[Int] = {
    val smallFactors = (1 to Math.sqrt(x).toInt)
      .filter(d => x % d == 0)
      .toSet

    val largeFactors = smallFactors.map(d => x / d)

    smallFactors union largeFactors
  }

  /**
   * Compute the extended Euclidean algorithm for two integers
   * @param a First input
   * @param b Second input
   * @return The gcd, and bézout coefficients of a and b, in order
   */
  def extendedEuclidean(a: BigInt, b: BigInt): (BigInt, BigInt, BigInt) = {
    @tailrec
    def helper(oldR: BigInt, r: BigInt, oldS: BigInt, s: BigInt, oldT: BigInt, t: BigInt): (BigInt, BigInt, BigInt) = {
      if (r == 0) (oldR, oldS, oldT)
      else {
        val quotient = oldR / r
        helper(r, oldR - quotient * r, s, oldS - quotient * s, t, oldT - quotient * t)
      }
    }

    if (a < b) {
      val result = extendedEuclidean(b, a)
      result.copy(_2 = result._3, _3 = result._2)
    }
    else helper(a, b, 1, 0, 0, 1)
  }

  def chineseRemainderTheorem(pairs: Vector[(BigInt, BigInt)]): BigInt = {
    val (offset, multiplier) = pairs.reduce { (acc, next) =>
      val (accOffset, accMod) = acc
      val (nextOffset, nextMod) = next
      val (_, accBezout, nextBezout) = Integers.extendedEuclidean(accMod, nextMod)
      val result = accOffset * nextMod * nextBezout + nextOffset * accMod * accBezout
      (result, acc._2 * next._2)
    }

    val possibleResult = offset % multiplier
    if (possibleResult < 0) possibleResult + multiplier else possibleResult
  }

  def chineseRemainderTheoremSieve(pairs: Vector[(Long, Long)]): Long = {
    def sequence(head: Long, increment: Long): LazyList[Long] = {
      head #:: sequence(head + increment, increment)
    }

    @tailrec
    def helper(current: Long, increment: Long, remainingPairs: Vector[(Long, Long)]): Long = {
      if (remainingPairs.isEmpty) current
      else {
        val (remainder, mod) = remainingPairs.head
        val nextAmount = sequence(current, increment).find(x => x % mod == remainder).get
        helper(nextAmount, increment * mod, remainingPairs.tail)
      }
    }

    val sorted = pairs.sorted(Ordering.by[(Long, Long), Long](-_._2))

    helper(0, 1, sorted)
  }

}

