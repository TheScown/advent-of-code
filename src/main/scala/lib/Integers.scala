package space.scown.adventofcode
package lib

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

}

