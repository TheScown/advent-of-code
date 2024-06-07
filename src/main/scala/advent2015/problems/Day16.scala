package space.scown.adventofcode
package advent2015.problems

import lib.{Files, Problem}

case class Day16(input: Vector[String]) extends Problem {
  private val requiredAttributes = Map(
    "children" -> 3,
    "cats" -> 7,
    "samoyeds" -> 2,
    "pomeranians" -> 3,
    "akitas" -> 0,
    "vizslas" -> 0,
    "goldfish" -> 5,
    "trees" -> 3,
    "cars" -> 2,
    "perfumes" -> 1,
  )

  override def solve1(): Unit = {
    val sues = parse()

    val result = sues.find(s => s.attributes.forall(p => p._2 == requiredAttributes(p._1))).get.id

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val sues = parse()

    val result = sues.find(s => s.attributes.forall {
      case ("cats", v) => v > requiredAttributes("cats")
      case ("trees", v) => v > requiredAttributes("trees")
      case ("pomeranians", v) => v < requiredAttributes("pomeranians")
      case ("goldfish", v) => v < requiredAttributes("goldfish")
      case (k, v) => v == requiredAttributes(k)
    }).get.id

    println(s"Result 2: $result")
  }

  def parse(): Vector[Sue] = {
    val pattern = "Sue (\\d+): ([a-z]+): (\\d+), ([a-z]+): (\\d+), ([a-z]+): (\\d+)".r

    input.map {
      case pattern(id, key1, value1, key2, value2, key3, value3) => Sue(
        id.toInt,
        Map(
          key1 -> value1.toInt,
          key2 -> value2.toInt,
          key3 -> value3.toInt
        )
      )
    }
  }

  case class Sue(id: Int, attributes: Map[String, Int])
}

case object Day16 extends App {
  val input = Files.lines("2015/day16.txt")
  val problem = Day16(input)
  problem.solve1()
  problem.solve2()
}
