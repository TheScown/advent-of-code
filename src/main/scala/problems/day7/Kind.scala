package space.scown.advent2023
package problems.day7

sealed trait Kind extends Ordered[Kind] {

  val rank: Int

  def compare(that: Kind): Int = {
    rank - that.rank
  }

}

case object FiveKind extends Kind {
  override val rank = 6
}

case object FourKind extends Kind {
  override val rank = 5
}

case object FullHouse extends Kind {
  override val rank = 4
}

case object ThreeKind extends Kind {
  override val rank = 3
}

case object TwoPair extends Kind {
  override val rank = 2
}

case object Pair extends Kind {
  override val rank = 1
}

case object HighCard extends Kind {
  override val rank = 0
}
