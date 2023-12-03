package space.scown.advent2023
package problems.day3

import scala.language.postfixOps
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.token.Tokens
import scala.util.parsing.input.Positional

case object Grammar extends RegexParsers {

  def token: Parser[Token] = (number
    | dot
    | star
    | at
    | equals
    | percent
    | plus
    | dollar
    | ampersand
    | slash
    | minus
    | hash)
  def tokens: Parser[Seq[Token]] = token*

  def number: Parser[Token] = positioned("\\d+".r ^^ (value => Number(value)))
  def dot: Parser[Token] = positioned("\\.".r ^^ (_ => Dot()))
  def star: Parser[Token] = positioned("\\*".r ^^ (_ => Star()))
  def at: Parser[Token] = positioned("@".r ^^ (_ => At()))
  def equals: Parser[Token] = positioned("=".r ^^ (_ => Equals()))
  def percent: Parser[Token] = positioned("%".r ^^ (_ => Percent()))
  def plus: Parser[Token] = positioned("\\+".r ^^ (_ => Plus()))
  def dollar: Parser[Token] = positioned("\\$".r ^^ (_ => Dollar()))
  def ampersand: Parser[Token] = positioned("&".r ^^ (_ => Ampersand()))
  def slash: Parser[Token] = positioned("/".r ^^ (_ => Slash()))
  def minus: Parser[Token] = positioned("-".r ^^ (_ => Minus()))
  def hash: Parser[Token] = positioned("#".r ^^ (_ => Hash()))

}

sealed trait Token extends Tokens with Positional
case class Number(value: String) extends Token {
  override def equals(o: Any): Boolean = super.equals(o)

  override def hashCode: Int = super.hashCode
}
case class Dot() extends Token
case class Star() extends Token
case class At() extends Token
case class Equals() extends Token
case class Percent() extends Token
case class Plus() extends Token
case class Dollar() extends Token
case class Ampersand() extends Token
case class Slash() extends Token
case class Minus() extends Token
case class Hash() extends Token
