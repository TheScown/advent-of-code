package space.scown.adventofcode
package advent2019.problems

import lib.{Files, Integers, Problem, Vec3}

import scala.annotation.tailrec

case class Day12(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val moons = parse()

    val finalMoons = (0 until 1000).foldLeft(moons) { (moons, _) =>
      step(moons)
    }

    val result = finalMoons.map(_.energy).sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val initialMoons = parse()

    @tailrec
    def helper(current: Vector[Moon], knownCycles: Map[Int, Long], count: Int): Long = {
      if (knownCycles.size == 3)
        knownCycles.values.reduce(Integers.lcm[Long])
      else {
        val newMoons = step(current)
        val zippedMoons = newMoons.zip(initialMoons)

        val updatedKnownCycles = (0 until 3).foldLeft(knownCycles) { (knownCycles, i) =>
          if (knownCycles.contains(i)) knownCycles
          else {
            val hasCycled = zippedMoons.forall { case (newMoon, initialMoon) =>
              newMoon.position(i) == initialMoon.position(i) && newMoon.velocity(i) == 0
            }

            if (hasCycled)
              knownCycles + (i -> (count + 1L))
            else knownCycles
          }
        }

        helper(newMoons, updatedKnownCycles, count + 1)
      }
    }

    val result = helper(initialMoons, Map(), 0)

    println(s"Result 2: $result")
  }

  private def step(moons: Vector[Moon]): Vector[Moon] = {
    moons.map { moon =>
      val otherMoons = moons.filter(_ ne moon)
      val afterVelocityUpdate = otherMoons.foldLeft(moon) { (moon, otherMoon) =>
        val newVelocity = moon.position.map(otherMoon.position, moon.velocity) { (position, otherPosition, velocity) =>
          if (position < otherPosition) velocity + 1
          else if (position > otherPosition) velocity - 1
          else velocity
        }

        moon.copy(
          velocity = newVelocity
        )
      }

      afterVelocityUpdate.copy(
        position = moon.position + afterVelocityUpdate.velocity
      )
    }
  }

  def parse(): Vector[Moon] = {
    val number = "(0|-?[1-9][0-9]*)"
    val pattern = s"<x=$number, y=$number, z=$number>".r

    input.zipWithIndex.map {
      case (pattern(p1, p2, p3), id) => Moon(
        id = id,
        position = Vec3(p1.toInt, p2.toInt, p3.toInt),
        velocity = Vec3(0, 0, 0)
      )
    }
  }

  case class Moon(id: Int, position: Vec3[Int], velocity: Vec3[Int]) {
    def energy: Int = position.norm1 * velocity.norm1
  }
}

case object Day12 extends App {
  val input = Files.lines("2019/day12.txt")
  val problem = Day12(input)
  problem.solve1()
  problem.solve2()
}
