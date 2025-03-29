package space.scown.adventofcode
package intcodeassembler

import scala.language.postfixOps
import scala.util.parsing.combinator.RegexParsers

case object Grammar extends RegexParsers {

  def lines: Parser[List[Instruction]] = line*

  def line: Parser[Instruction] = label | value | instruction

  def instruction: Parser[Instruction] = add | mul | in | out | jez | jnz | lt | eq | rb | end

  def add: Parser[Add] = "ADD" ~> repNM(3, 3, parameter, ",") ^^ (params => Add(params.head, params(1), params(2)))
  def mul: Parser[Mul] = "MUL" ~> repNM(3, 3, parameter, ",") ^^ (params => Mul(params.head, params(1), params(2)))
  def in: Parser[In] = "IN" ~> repNM(1, 1, parameter, ",") ^^ (params => In(params.head))
  def out: Parser[Out] = "OUT" ~> repNM(1, 1, parameter, ",") ^^ (params => Out(params.head))
  def jez: Parser[Jez] = "JEZ" ~> repNM(2, 2, parameter, ",") ^^ (params => Jez(params.head, params(1)))
  def jnz: Parser[Jnz] = "JNZ" ~> repNM(2, 2, parameter, ",") ^^ (params => Jnz(params.head, params(1)))
  def lt: Parser[Lt] = "LT" ~> repNM(3, 3, parameter, ",") ^^ (params => Lt(params.head, params(1), params(2)))
  def eq: Parser[Eq] = "EQ" ~> repNM(3, 3, parameter, ",") ^^ (params => Eq(params.head, params(1), params(2)))
  def rb: Parser[Rb] = "RB" ~> repNM(1, 1, parameter, ",") ^^ (params => Rb(params.head))
  def end: Parser[End] = "END" ^^ (_ => End())
  def value: Parser[Value] = int ^^ (i => Value(i))

  def parameter: Parser[Parameter] = labelParameter | valueParameter

  def labelParameter: Parser[LabelParameter] = (addressingMode ~ labelRef ~ opt("[+-]".r ~> int)) ^^ {
    case mode ~ label ~ None => LabelParameter(label, mode)
    case mode ~ label ~ Some(offset) => LabelParameter(label, mode, offset)
  }

  def valueParameter: Parser[ValueParameter] = addressingMode ~ int ^^ {
    case mode ~ value => ValueParameter(value, mode)
  }

  def int: Parser[Long] = "0|[+-]?[1-9][0-9]*".r ^^ (_.toLong)

  def addressingMode: Parser[AddressingMode] = "[pir]".r ^^ {
    case "p" => Positioned
    case "i" => Immediate
    case "r" => Relative
  }

  def label: Parser[Label] = labelRef <~ ":" ^^ (s => Label(s))
  def labelRef: Parser[String] = "[a-z_]+".r

}
