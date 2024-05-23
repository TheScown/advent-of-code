package space.scown.adventofcode
package advent2015.problems.day7

sealed trait Operation

case class Fixed(c: Char) extends Operation

case class Passthrough(name: String) extends Operation

case class Not(name: String) extends Operation

case class And(left: String, right: String) extends Operation

case class FixedAnd(left: Char, right: String) extends Operation

case class Or(left: String, right: String) extends Operation

case class LShift(left: String, right: Char) extends Operation

case class RShift(left: String, right: Char) extends Operation
