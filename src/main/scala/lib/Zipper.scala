package space.scown.adventofcode
package lib

import scala.annotation.tailrec

case object Zipper {
  sealed trait Tree[T]
  case class Leaf[T](val value: T) extends Tree[T]
  case class Fork[T](val left: Tree[T], val right: Tree[T]) extends Tree[T]

  sealed trait Context[T]
  case class Top[T]() extends Context[T]
  case class L[T](parent: Context[T], right: Tree[T]) extends Context[T]
  case class R[T](left: Tree[T], parent: Context[T]) extends Context[T]

  case class Location[T](tree: Tree[T], context: Context[T])

  def left[T](location: Location[T]): Location[T] = location match {
    case Location(Fork(l, r), context) => Location(l, L(context, r))
  }

  def right[T](location: Location[T]): Location[T] = location match {
    case Location(Fork(l, r), context) => Location(r, R(l, context))
  }

  def top[T](tree: Tree[T]): Location[T] = Location(tree, Top())

  def up[T](location: Location[T]): Location[T] = location match {
    case Location(tree, L(context, r)) => Location(Fork(tree, r), context)
    case Location(tree, R(l, context)) => Location(Fork(l, tree), context)
  }

  @tailrec
  def upmost[T](location: Location[T]): Location[T] = location match {
    case Location(_, Top()) => location
    case _ => upmost(up(location))
  }

  def modify[T](location: Location[T], f: Tree[T] => Tree[T]): Location[T] = location match {
    case Location(tree, context) => Location(f(tree), context)
  }
}
