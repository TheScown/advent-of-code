package space.scown.adventofcode
package advent2016.assembunny

sealed trait Instruction
case class Copy(source: Source, dest: Register) extends Instruction
case class Inc(register: Register) extends Instruction
case class Dec(register: Register) extends Instruction
case class Jnz(source: Source, jump: Value) extends Instruction

sealed trait Source
case class Register(name: Char) extends Source
case class Value(number: Int) extends Source
