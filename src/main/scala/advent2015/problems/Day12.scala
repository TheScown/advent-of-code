package space.scown.adventofcode
package advent2015.problems

import lib.{Files, Problem}

import org.json4s.{JDecimal, JDouble, JInt, JLong, JString}
import org.json4s.JsonAST.{JArray, JObject, JValue}
import org.json4s.native.JsonParser

case class Day12(input: String) extends Problem {
  override def solve1(): Unit = {
    val pattern = "-?\\d+".r

    val result = pattern.findAllIn(input).map(_.toInt).sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    def sumJson(value: JValue): Int = value match {
      case JInt(num) => num.toInt
      case JDouble(num) => num.toInt
      case JDecimal(num) => num.toInt
      case JLong(num) => num.toInt
      case JArray(items) => items.map(sumJson).sum
      case JObject(fields) =>
        if (fields.exists(field => field._2 == JString("red"))) 0
        else fields.map(_._2).map(sumJson).sum
      case _ => 0
    }

    val result = sumJson(JsonParser.parse(input))

    println(s"Result 2: $result")
  }
}

case object Day12 extends App {
  val input = Files.lines("2015/day12.txt")(0)
  val problem = Day12(input)
  problem.solve1()
  problem.solve2()
}
