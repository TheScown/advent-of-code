package space.scown.adventofcode
package advent2019.intcode

sealed trait PositionMode {
  def apply(pc: Int, memory: Memory): Long

  def dest(pc: Int, memory: Memory): Int
}

object PositionMode {

  def fromInt(p: Int, offset: Int, relativeBase: Int): PositionMode = p match {
    case 0 => Position(offset)
    case 1 => Immediate(offset)
    case 2 => Relative(offset, relativeBase)
    case _ => throw new IllegalStateException(s"Invalid position mode $p")
  }

}

case class Position(offset: Int) extends PositionMode {
  override def apply(pc: Int, memory: Memory): Long = {
    val pointer = memory(pc + offset)
    memory(pointer.toInt)
  }

  override def dest(pc: Int, memory: Memory): Int = {
    memory(pc + offset).toInt
  }
}

case class Immediate(offset: Int) extends PositionMode {
  override def apply(pc: Int, memory: Memory): Long = {
    memory(pc + offset)
  }

  override def dest(pc: Int, memory: Memory): Int = {
    throw new IllegalStateException(s"Attempting to write in immediate mode. $pc + $offset, $memory")
  }
}

case class Relative(offset: Int, relativeBase: Int) extends PositionMode {
  override def apply(pc: Int, memory: Memory): Long = {
    val pointer = memory(pc + offset).toInt
    memory(pointer + relativeBase)
  }

  override def dest(pc: Int, memory: Memory): Int = {
    memory(pc + offset).toInt + relativeBase
  }
}