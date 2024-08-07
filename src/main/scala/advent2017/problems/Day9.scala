package space.scown.adventofcode
package advent2017.problems

import lib.{Files, Problem}

import scala.language.postfixOps
import scala.util.parsing.combinator.RegexParsers

case class Day9(input: String) extends Problem {
  override def solve1(): Unit = {
    val topGroup = parse()

    def helper(group: Group, depth: Int): Int = {
      if (group.children.isEmpty) depth
      else depth + group.children.map {
        case g@Group(_) => helper(g, depth + 1)
        case Garbage(_) => 0
      }.sum
    }

    val result = helper(topGroup, 1)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val topGroup = parse()

    def helper(group: Group): Int = {
      if (group.children.isEmpty) 0
      else group.children.map {
        case g@Group(_) => helper(g)
        case Garbage(s) => s.length
      }.sum
    }

    val result = helper(topGroup)

    println(s"Result 2: $result")
  }

  private def parse() = {
    Grammar.parse[Group](Grammar.group, input) match {
      case Grammar.Success(group, _) => group
      case Grammar.NoSuccess.I(err, _) => throw new IllegalStateException(err)
    }
  }

  case object Grammar extends RegexParsers {
    def group: Parser[Group] = ("{" ~> (stuffCsv?) <~ "}") ^^ {
      case Some(v) => Group(v)
      case None => Group(Vector())
    }

    private def stuffCsv: Parser[Vector[Stuff]] = repsep(stuff, ",") ^^ (x => x.toVector)

    private def stuff: Parser[Stuff] = garbage | group

    private def garbage: Parser[Garbage] = ("<" ~> (garbageToken*) <~ ">") ^^ (garbage => Garbage(garbage.filter(_.head != '!').mkString("")))

    private def garbageToken: Parser[String] = "!.".r | "[^>]".r
  }

  trait Stuff
  case class Garbage(garbage: String) extends Stuff
  case class Group(children: Vector[Stuff]) extends Stuff
}

case object Day9 extends App {
  val input = Files.lines("2017/day9.txt").head
  val problem = Day9(input)
  problem.solve1()
  problem.solve2()
}
