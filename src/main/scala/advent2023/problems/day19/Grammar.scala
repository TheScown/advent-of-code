package space.scown.adventofcode
package advent2023.problems.day19

import scala.annotation.tailrec
import scala.language.postfixOps
import scala.util.parsing.combinator.RegexParsers

case object Grammar extends RegexParsers {

  def functions: Parser[Seq[Function]] = function*

  def function: Parser[Function] = ((((name <~ '{') ~ conditionList) <~ ',') ~ body <~ '}') ^^ {
    case name ~ conditionList ~ defaultBody => Function(name, conditionList, defaultBody)
  }

  def inputs: Parser[Seq[Variables]] = variables*

  def variables: Parser[Variables] = ('{' ~> repsep((variable <~ '=') ~ number, ',') <~ '}') ^^ { list =>
    @tailrec
    def helper(list: List[String ~ Int], input: Variables): Variables = list match {
      case Nil => input
      case (variable ~ value) :: tail => helper(tail, input.withVariable(variable, value))
    }

    helper(list, Variables())
  }

  def conditionList: Parser[List[ConditionWithBody]] = repsep(conditionWithBody, ',')

  def conditionWithBody: Parser[ConditionWithBody] = (condition <~ ':') ~ body  ^^ {
    case condition ~ body => ConditionWithBody(condition, body)
  }

  def condition: Parser[Condition] = variable ~ "[<>]".r ~ number ^^ {
    case name ~ "<" ~ number => LessThan(name, number)
    case name ~ ">" ~ number => GreaterThan(name, number)
  }

  def body: Parser[Body] = accept | reject | workflow

  def workflow: Parser[Body] = name ^^ (name => Workflow(name))

  def name: Parser[String] = "[a-z]+".r

  def accept: Parser[Body] = "A".r ^^ (_ => Accept)

  def reject: Parser[Body] = "R".r ^^ (_ => Reject)

  def variable: Parser[String] = "[xmas]".r

  def number: Parser[Int] = "[1-9][0-9]*".r ^^ (s => s.toInt)

}

case class Function(name: String, conditions: List[ConditionWithBody], defaultBody: Body) {
  def apply(functions: Map[String, Function], variables: Variables): Boolean = {
    conditions.find(c => c.condition(variables)) match {
      case Some(ConditionWithBody(_, body)) => body(functions, variables)
      case None => defaultBody(functions, variables)
    }
  }
}

case class ConditionWithBody(condition: Condition, body: Body)

sealed trait Condition {
  def apply(variables: Variables): Boolean

  def complement: Condition
}

case class LessThan(variable: String, value: Int) extends Condition {
  override def apply(variables: Variables): Boolean = variables(variable) < value

  override def complement: Condition = GreaterThan(variable, value - 1)
}

case class GreaterThan(variable: String, value: Int) extends Condition {
  override def apply(variables: Variables): Boolean = variables(variable) > value

  override def complement: Condition = LessThan(variable, value + 1)
}

case object AlwaysTrue extends Condition {

  override def apply(variables: Variables): Boolean = true

  override def complement: Condition = AlwaysFalse
}

case object AlwaysFalse extends Condition {

  override def apply(variables: Variables): Boolean = false

  override def complement: Condition = AlwaysTrue
}

sealed trait Body {
  def apply(functions: Map[String, Function], variables: Variables): Boolean
}

case object Accept extends Body {
  override def apply(functions: Map[String, Function], variables: Variables): Boolean = true
}
case object Reject extends Body {
  override def apply(functions: Map[String, Function], variables: Variables): Boolean = false
}

case class Workflow(name: String) extends Body {
  override def apply(functions: Map[String, Function], variables: Variables): Boolean = functions(name)(functions, variables)
}

case class Variables(x: Int = 0, m: Int = 0, a: Int = 0, s: Int = 0) {
  def apply(variable: String): Int = variable match {
    case "x" => x
    case "m" => m
    case "a" => a
    case "s" => s
    case _ => throw new IllegalStateException(s"Invalid variable $variable")
  }

  def withVariable(variable: String, value: Int): Variables = variable match {
    case "x" => copy(x = value)
    case "m" => copy(m = value)
    case "a" => copy(a = value)
    case "s" => copy(s = value)
    case _ => throw new IllegalStateException(s"Invalid variable $variable")
  }

  def sum: Int = {
    x + m + a + s
  }

  def product: BigInt = {
    BigInt(x) * BigInt(m) * BigInt(a) * BigInt(s)
  }

  def -(other: Variables): Variables = {
    Variables(x - other.x + 1, m - other.m + 1, a - other.a + 1, s - other.s + 1)
  }
}