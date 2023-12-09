package space.scown.adventofcode2019
package intcode

object IntcodeProgram {

  def fromString(input: String): Vector[Long] = {
    input.split(",").map(_.toLong).toVector
  }

  def fromLines(input: Vector[String]): Vector[Long] = {
    IntcodeProgram.fromString(input.mkString(""))
  }

}
