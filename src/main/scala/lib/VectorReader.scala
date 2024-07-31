package space.scown.adventofcode
package lib

import scala.util.parsing.input.{NoPosition, Position, Reader}

/**
 * Reader backed by a Vector. Useful for writing an optimising grammar.
 *
 * @param input The vector of things to parse
 * @tparam T The type of the vector
 */
case class VectorReader[T](input: Vector[T]) extends Reader[T] {
  override def first: T = input.head

  override def rest: Reader[T] = VectorReader(input.tail)

  override def pos: Position = NoPosition

  override def atEnd: Boolean = input.isEmpty
}
