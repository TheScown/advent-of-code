package space.scown.adventofcode2019
package intcode

case class IntcodeComputer(program: Vector[Long]) {

  def execute(input: LazyList[Long] = LazyList()): LazyList[(Memory, Option[Long])] = {
    def helper(pc: Int, memory: Memory, input: LazyList[Long], relativeBase: Int): LazyList[(Memory, Option[Long])] = {
//      println(s"$pc, $relativeBase, $memory")

      Instruction.fromInt(memory(pc).toInt, relativeBase) match {
        case Instruction(1, positionModes) => helper(pc + 4, add(pc, memory, positionModes), input, relativeBase)
        case Instruction(2, positionModes) => helper(pc + 4, multiply(pc, memory, positionModes), input, relativeBase)
        case Instruction(3, positionModes) =>
          val (updatedMemory, remainingInput) = readIn(pc, memory, positionModes, input)
          helper(pc + 2, updatedMemory, remainingInput, relativeBase)
        case Instruction(4, positionModes) =>
          (memory, Some(positionModes(0)(pc, memory))) #:: helper(pc + 2, memory, input, relativeBase)
        case Instruction(5, positionModes) => helper(jumpTrue(pc, memory, positionModes), memory, input, relativeBase)
        case Instruction(6, positionModes) => helper(jumpFalse(pc, memory, positionModes), memory, input, relativeBase)
        case Instruction(7, positionModes) => helper(pc + 4, lessThan(pc, memory, positionModes), input, relativeBase)
        case Instruction(8, positionModes) => helper(pc + 4, equal(pc, memory, positionModes), input, relativeBase)
        case Instruction(9, positionModes) => helper(pc + 2, memory, input, updateRelativeBase(pc, memory, positionModes, relativeBase))
        case Instruction(99, _) => LazyList((memory, None))
        case x => throw new IllegalStateException(s"Illegal opcode $x at $pc. State: $memory")
      }
    }

    helper(0, Memory(program), input, 0)
  }

  private def add(pc: Int, memory: Memory, positionModes: Vector[PositionMode]): Memory = {
    memory(positionModes(2).dest(pc, memory), positionModes(0)(pc, memory) + positionModes(1)(pc, memory))
  }

  private def multiply(pc: Int, memory: Memory, positionModes: Vector[PositionMode]): Memory = {
    memory(positionModes(2).dest(pc, memory), positionModes(0)(pc, memory) * positionModes(1)(pc, memory))
  }

  private def readIn(pc: Int, memory: Memory, positionModes: Vector[PositionMode], input: LazyList[Long]): (Memory, LazyList[Long]) = {
    input match {
      case next #:: remainder => (memory(positionModes(0).dest(pc, memory), next), remainder)
      case LazyList() => throw new IllegalStateException(s"No more input. $pc, $memory")
    }
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
