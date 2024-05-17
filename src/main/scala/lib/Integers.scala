package space.scown.adventofcode
package lib

object Integers {

  def naturalNumbers: LazyList[Int] = {
    def next(n: Int): LazyList[Int] = {
      n #:: next(n + 1)
    }

    next(1)
  }

}
