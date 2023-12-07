package space.scown.advent2023
package problems.day7

sealed trait Card extends Ordered[Card] {
  val rank1: Int
  val rank2: Int

  def compare(that: Card): Int = {
    rank1 - that.rank1
  }
}
case object Ace extends Card {
  override val rank1 = 14
  override val rank2 = 14
}

case object King extends Card {
  override val rank1 = 13
  override val rank2 = 13
}

case object Queen extends Card {
  override val rank1 = 12
  override val rank2 = 12
}

case object Jack extends Card {
  override val rank1 = 11
  override val rank2 = 1
}

case object Ten extends Card {
  override val rank1 = 10
  override val rank2 = 10
}

case class Number(rank1: Int) extends Card {
  override val rank2: Int = rank1
}
