package space.scown.adventofcode
package advent2017.problems

import lib.{Files, Problem, Timer}

import scala.annotation.tailrec

case class Day7(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val discs = parse()
    val names = discs.map(_.name).toSet
    val supportedDiscs = discs.flatMap(_.supports).toSet
    val result = names.find(name => !supportedDiscs.contains(name)).get

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val discs = parse()
    val names = discs.map(_.name).toSet
    val supportedDiscs = discs.flatMap(_.supports).toSet
    val base = names.find(name => !supportedDiscs.contains(name)).get
    val index = discs.map(d => d.name -> d).toMap

    @tailrec
    def getSupports(names: Vector[String], acc: Vector[(Disc, WeightResult)] = Vector()): Vector[(Disc, WeightResult)] = {
      if (names.isEmpty) acc
      else {
        val next = names.head
        val disc = index(next)
        helper(disc) match {
          case delta@Delta(_) =>
            Vector((disc, delta))
          case weight@TowerWeight(_) => getSupports(names.tail, acc :+ (disc, weight))
        }
      }
    }

    def helper(disc: Disc): WeightResult = {
      if (disc.supports.isEmpty) TowerWeight(disc.weight)
      else {
        val supportedWeights = getSupports(disc.supports)

        if (!supportedWeights.head._2.intermediate) supportedWeights.head._2
        else {
          val distinctWeights = supportedWeights.groupBy(_._2.weight)

          if (distinctWeights.size == 1) TowerWeight(disc.weight + supportedWeights.map(_._2.weight).sum)
          else {
            val oddOneOut = distinctWeights.minBy(_._2.size)._2.head
            val majority = distinctWeights.maxBy(_._2.size)._2.head._2.weight

            Delta(majority - oddOneOut._2.weight + oddOneOut._1.weight)
          }
        }
      }
    }

    val result = helper(index(base)).weight

    println(s"Result 2: $result")
  }

  private def parse(): Vector[Disc] = {
    input.map { line =>
      val parts = line.split(" -> ")
      val lhs = parts.head
      val supports = if (parts.tail.isEmpty) Vector[String]() else parts(1).split(",").map(_.trim).toVector
      val pattern = "([a-z]+) \\((\\d+)\\)".r
      lhs match {
        case pattern(name, weight) => Disc(name, weight.toInt, supports)
      }
    }
  }

  case class Disc(name: String, weight: Int, supports: Vector[String])

  private sealed trait WeightResult {
    val intermediate: Boolean
    val weight: Int
  }
  private case class TowerWeight(weight: Int) extends WeightResult {
    override val intermediate: Boolean = true
  }
  private case class Delta(weight: Int) extends WeightResult {
    override val intermediate: Boolean = false
  }
}

case object Day7 extends App {
  val input = Files.lines("2017/day7.txt")
  val problem = Day7(input)
  problem.solve1()
  Timer.time(() => problem.solve2())
}
