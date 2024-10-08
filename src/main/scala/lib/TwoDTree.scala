package space.scown.adventofcode
package lib

import lib.TwoDTree.Orientation

case class TwoDTree(point: Complex[Int], orientation: Orientation, left: Option[TwoDTree], right: Option[TwoDTree]) {
  def nearest(p: Complex[Int]): Complex[Int] = nearest(p, point)

  private def nearest(p: Complex[Int], best: Complex[Int]): Complex[Int] = {
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
    val comparator: Complex[Int] => Int
  }
  case object Vertical extends Orientation {
    val flip: Orientation = Horizontal
    override val comparator: Complex[Int] => Int = _.re
  }
  case object Horizontal extends Orientation {
    val flip: Orientation = Vertical
    override val comparator: Complex[Int] => Int = _.im
  }

  def of(points: Vector[Complex[Int]]): TwoDTree = of(points, Vertical)

  private def of(points: Vector[Complex[Int]], orientation: Orientation): TwoDTree = {
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
