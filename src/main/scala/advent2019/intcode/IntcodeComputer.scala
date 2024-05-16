package space.scown.adventofcode
package advent2019.intcode

case class IntcodeComputer(program: Vector[Long]) {

  def execute(): Output = {
    def helper(pc: Int, memory: Memory, relativeBase: Int, outputs: Seq[Long]): Output = {
//      println(s"$pc, $relativeBase, $memory")

      Instruction.fromInt(memory(pc).toInt, relativeBase) match {
        case Instruction(1, positionModes) => helper(pc + 4, add(pc, memory, positionModes), relativeBase, outputs)
        case Instruction(2, positionModes) => helper(pc + 4, multiply(pc, memory, positionModes), relativeBase, outputs)
        case Instruction(3, positionModes) => RequiresInput(outputs, next => {
//          println(s"Reading $next")
          val updatedMemory = readIn(pc, memory, positionModes, next)
          helper(pc + 2, updatedMemory, relativeBase, Vector())
        })
        case Instruction(4, positionModes) => {
          val outputValue = positionModes(0)(pc, memory)
//          println(s"Output value $outputValue")
          helper(pc + 2, memory, relativeBase, outputs :+ outputValue)
        }
        case Instruction(5, positionModes) => helper(jumpTrue(pc, memory, positionModes), memory, relativeBase, outputs)
        case Instruction(6, positionModes) => helper(jumpFalse(pc, memory, positionModes), memory, relativeBase, outputs)
        case Instruction(7, positionModes) => helper(pc + 4, lessThan(pc, memory, positionModes), relativeBase, outputs)
        case Instruction(8, positionModes) => helper(pc + 4, equal(pc, memory, positionModes), relativeBase, outputs)
        case Instruction(9, positionModes) => helper(pc + 2, memory, updateRelativeBase(pc, memory, positionModes, relativeBase), outputs)
        case Instruction(99, _) => Termination(outputs, memory)
        case x => throw new IllegalStateException(s"Illegal opcode $x at $pc. State: $memory")
      }
    }

    helper(0, Memory(program), 0, Vector())
  }

  private def add(pc: Int, memory: Memory, positionModes: Vector[PositionMode]): Memory = {
    memory(positionModes(2).dest(pc, memory), positionModes(0)(pc, memory) + positionModes(1)(pc, memory))
  }

  private def multiply(pc: Int, memory: Memory, positionModes: Vector[PositionMode]): Memory = {
    memory(positionModes(2).dest(pc, memory), positionModes(0)(pc, memory) * positionModes(1)(pc, memory))
  }

  private def readIn(pc: Int, memory: Memory, positionModes: Vector[PositionMode], next: Long): Memory = {
    memory(positionModes(0).dest(pc, memory), next)
  }

  private def jumpTrue(pc: Int, memory: Memory, positionModes: Vector[PositionMode]): Int = {
    val test = positionModes(0)(pc, memory)

    if (test != 0) positionModes(1)(pc, memory).toInt
    else pc + 3
  }

  private def jumpFalse(pc: Int, memory: Memory, positionModes: Vector[PositionMode]): Int = {
    val test = positionModes(0)(pc, memory)

    if (test == 0) positionModes(1)(pc, memory).toInt
    else pc + 3
  }

  private def lessThan(pc: Int, memory: Memory, positionModes: Vector[PositionMode]): Memory = {
    val condition = positionModes(0)(pc, memory) < positionModes(1)(pc, memory)
    memory(positionModes(2).dest(pc, memory), if (condition) 1 else 0)
  }

  private def equal(pc: Int, memory: Memory, positionModes: Vector[PositionMode]): Memory = {
    val condition = positionModes(0)(pc, memory) == positionModes(1)(pc, memory)
    memory(positionModes(2).dest(pc, memory), if (condition) 1 else 0)
  }

  private def updateRelativeBase(pc: Int, memory: Memory, positionModes: Vector[PositionMode], relativeBase: Int): Int = {
    relativeBase + positionModes(0)(pc, memory).toInt
  }
}

case class Memory(memory: Vector[Long]) {

  def apply(index: Int): Long = {
    if (index >= memory.size) 0.toLong
    else memory(index)
  }

  def apply(index: Int, newValue: Long): Memory = {
    val toUpdate = if (index >= memory.size) memory ++ Vector.fill(index + 1 - memory.size)(0.toLong) else memory
    Memory(toUpdate.updated(index, newValue))
  }

}

sealed trait Output {
  val terminal: Boolean
  val outputs: Seq[Long]
}

case class Termination(outputs: Seq[Long], memory: Memory) extends Output {
  override val terminal: Boolean = true
}

case class RequiresInput(outputs: Seq[Long], continue: Long => Output) extends Output {
  override val terminal: Boolean = false
}
