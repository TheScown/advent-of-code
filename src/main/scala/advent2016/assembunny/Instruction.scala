package space.scown.adventofcode
package advent2016.assembunny

sealed trait Instruction
case class Copy(source: Source, dest: Source) extends Instruction
case class Inc(register: Source) extends Instruction
case class Dec(register: Source) extends Instruction
case class Jnz(source: Source, jump: Source) extends Instruction
case class Tgl(source: Source) extends Instruction
case class Out(source: Source) extends Instruction
case class Add(source: Register, dest: Register) extends Instruction
case class Multiply(a: Register, b: Register, dest: Register) extends Instruction
case class Noop() extends Instruction

sealed trait Source
case class Register(name: Char) extends Source
case class Value(number: Int) extends Source
