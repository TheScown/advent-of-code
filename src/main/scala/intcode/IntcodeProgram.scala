package space.scown.adventofcode2019
package intcode

object IntcodeProgram {

  def fromString(input: String): Vector[Int] = {
    input.split(",").map(_.toInt).toVector
  }

  def fromLines(input: Vector[String]): Vector[Int] = {
    IntcodeProgram.fromString(input.mkString(""))
  }

}
