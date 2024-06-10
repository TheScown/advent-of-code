package space.scown.adventofcode
package advent2015.problems

import lib.{Files, Problem, Timer}

import scala.annotation.tailrec
import scala.language.postfixOps
import scala.util.Random
import scala.util.parsing.combinator.RegexParsers

case class Day19(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val sequences = parse() match {
      case Input(expandables, mapping, input) =>
        val tokenParser = parser(expandables)
        expandString(input, tokenParser, mapping)
    }

    val result = sequences.reduce(_ union _).size

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val result = parse() match {
      case Input(_, mapping, input) =>
        val reversedMapping = mapping.flatMap {
          case (k, vs) => vs.map(v => v -> k)
        }

        val replaceables = reversedMapping.keySet.toVector.sortBy(-_.length)
        val random = Random

        @tailrec
        def helper(s: String, count: Int): Int = {
          if (s == "e") count
          else {
            val firstReplaceable = random.shuffle(replaceables).find(s.indexOf(_) > -1)

            firstReplaceable match {
              case Some(needle) =>
                val newString = s.replaceFirst(needle, reversedMapping(needle))
                helper(newString, count + 1)
              case None => helper(input, 0)
            }
          }
        }

        helper(input, 0)
    }

    println(s"Result 2: $result")
  }

  private def parser(expandables: Set[String]): Tokenizer.Parser[Seq[Token]] = {
    val expandableParser = expandables.map(Tokenizer.expandable).reduce(_ | _)
    Tokenizer.rep1(expandableParser | Tokenizer.literal)
  }

  private def expandString(input: String, parser: Tokenizer.Parser[Seq[Token]], mapping: Map[String, Set[String]]): Seq[Set[String]] = {
    val tokens = tokenize(input, parser)

    tokens.zipWithIndex.map {
      case (Literal(_), _) => Set()
      case (Expandable(v), i) =>
        val expansions = mapping(v)
        expansions.map(e => tokens.map(_.value).updated(i, e).mkString(""))
    }
  }

  private def tokenize(input: String, parser: Tokenizer.Parser[Seq[Token]]): Seq[Token] = {
    Tokenizer.parseAll[Seq[Token]](parser, input) match {
      case Tokenizer.Success(tokens, _) => tokens
      case error@Tokenizer.Failure(_, _) => throw new IllegalArgumentException(s"$error")
      case Tokenizer.Error(msg, _) => throw new IllegalArgumentException(msg)
    }
  }

  private def parse(): Input = {
    val mappingLines = input.takeWhile(_.nonEmpty)
    val inputLine = input.dropWhile(_.nonEmpty).tail.head
    val mappingPattern = "([A-Za-z]+) => ([A-Za-z]+)".r

    val (expandables, mapping) = mappingLines.foldLeft((Set[String](), Map[String, Set[String]]())) {
      case (acc, line) => acc match {
        case (set, map) => line match {
          case mappingPattern(expandable, generated) =>
            val existingSet = map.getOrElse(expandable, Set())

            (set + expandable, map + (expandable -> (existingSet + generated)))
        }
      }
    }

    Input(expandables, mapping, inputLine)
  }

  private case class Input(
    expandables: Set[String],
    mapping: Map[String, Set[String]],
    inputString: String
  )

  private case object Tokenizer extends RegexParsers {
    def expandable(tokenString: String): Parser[Expandable] = tokenString ^^ Expandable

    def literal: Parser[Literal] = "[A-Za-z]".r ^^ Literal
  }

  private sealed trait Token {
    def value: String
    def isExpandable: Boolean
  }

  private case class Expandable(value: String) extends Token {
    override def isExpandable: Boolean = true
  }

  private case class Literal(value: String) extends Token {
    override def isExpandable: Boolean = false
  }
}

case object Day19 extends App {
  val input = Files.lines("2015/day19.txt")
  val problem = Day19(input)
  problem.solve1()
  Timer.time(() => problem.solve2())
}
