package space.scown.adventofcode
package lib

import lib.TwoDTree.Orientation

case class TwoDTree(point: Complex, orientation: Orientation, left: Option[TwoDTree], right: Option[TwoDTree]) {
  def nearest(p: Complex): Complex = nearest(p, point)

  private def nearest(p: Complex, best: Complex): Complex = {
    val currentDistance = p mh best
    val newDistance = p mh point

    val newBest = if (newDistance < currentDistance) point else best

    // TODO figure out how much subtree searching to do
    (left, right) match {
      case (Some(left), None) => left.nearest(p, newBest)
      case (None, Some(right)) => right.nearest(p, newBest)
      case (Some(left), Some(right)) =>
        val first = Seq(left, right).minBy(_.point mh p)
        val second = if (first == left) right else left
      case (None, None) => newBest
    }

    throw new UnsupportedOperationException("Implementation not finished")
  }
}

case object TwoDTree {
  sealed trait Orientation {
    val flip: Orientation
    val comparator: Complex => BigInt
  }
  case object Vertical extends Orientation {
    val flip: Orientation = Horizontal
    override val comparator: Complex => BigInt = _.re
  }
  case object Horizontal extends Orientation {
    val flip: Orientation = Vertical
    override val comparator: Complex => BigInt = _.im
  }

  def of(points: Vector[Complex]): TwoDTree = of(points, Vertical)

  private def of(points: Vector[Complex], orientation: Orientation): TwoDTree = {
    val pivot = points.head
    val (left, right) = points.partition(p => orientation.comparator(p) <= orientation.comparator(pivot))

    TwoDTree(
      pivot,
      orientation,
      if (left.nonEmpty) Some(TwoDTree.of(left, orientation.flip)) else None,
      if (right.nonEmpty) Some(TwoDTree.of(right, orientation.flip)) else None
    )
  }
}
