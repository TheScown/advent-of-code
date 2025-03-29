package space.scown.adventofcode
package intcodeassembler

sealed trait AddressingMode {
  def value: Int
}
case object Positioned extends AddressingMode {
  override def value: Int = 0
}
case object Immediate extends AddressingMode {
  override def value: Int = 1
}
case object Relative extends AddressingMode {
  override def value: Int = 2
}

sealed trait Parameter {
  def mode: AddressingMode
  def value: Long
}
case class ValueParameter(value: Long, mode: AddressingMode) extends Parameter
case class LabelParameter(label: String, mode: AddressingMode, offset: Long = 0L) extends Parameter {
  override def value: Long = throw new UnsupportedOperationException("Attempting to access value of LabelParameter")
}

sealed trait Instruction {
  def size: Int
}
case class Add(dest: Parameter, a: Parameter, b: Parameter) extends Instruction {
  override def size: Int = 4
}
case class Mul(dest: Parameter, a: Parameter, b: Parameter) extends Instruction {
  override def size: Int = 4
}
case class In(dest: Parameter) extends Instruction {
  override def size: Int = 2
}
case class Out(src: Parameter) extends Instruction {
  override def size: Int = 2
}
case class Jez(test: Parameter, dest: Parameter) extends Instruction {
  override def size: Int = 3
}
case class Jnz(test: Parameter, dest: Parameter) extends Instruction {
  override def size: Int = 3
}
case class Lt(dest: Parameter, a: Parameter, b: Parameter) extends Instruction {
  override def size: Int = 4
}
case class Eq(dest: Parameter, a: Parameter, b: Parameter) extends Instruction {
  override def size: Int = 4
}
case class Rb(value: Parameter) extends Instruction {
  override def size: Int = 2
}
case class End() extends Instruction {
  override def size: Int = 1
}

case class Label(value: String) extends Instruction {
  override def size: Int = 0
}

case class Value(value: Long) extends Instruction {
  override def size: Int = 1
}
