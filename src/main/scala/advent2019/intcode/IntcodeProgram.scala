package space.scown.adventofcode
package advent2019.intcode

object IntcodeProgram {

  def fromString(input: String): Vector[Long] = {
    input.split(",").map(_.toLong).toVector
  }

  def fromLines(input: Vector[String]): Vector[Long] = {
    IntcodeProgram.fromString(input.mkString(""))
  }

}
