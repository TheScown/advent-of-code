package space.scown.adventofcode2019
package intcode

case class Instruction(opcode: Long, positionModes: Vector[PositionMode])

object Instruction {

  def fromInt(rawOpcode: Int, relativeBase: Int): Instruction = {
//    println(s"$rawOpcode")
    val opcode = rawOpcode % 100
    val positionModeInt = (rawOpcode - opcode) / 100
    val positionModes = (1 to 3).map { m =>
      PositionMode.fromInt((positionModeInt / Math.pow(10, m - 1).toInt) % 10, m, relativeBase)
    }.toVector

    Instruction(opcode, positionModes)
  }

}