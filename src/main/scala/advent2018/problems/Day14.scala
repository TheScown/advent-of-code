package space.scown.adventofcode
package advent2018.problems

import lib.{Files, Problem}

import scala.annotation.tailrec

case class Day14(input: String) extends Problem {

  override def solve1(): Unit = {
    val target = input.toInt
    val requiredRecipes = target + 10

    @tailrec
    def helper(scoreboard: Vector[Int], aIndex: Int, bIndex: Int): String = {
      if (scoreboard.size >= requiredRecipes) scoreboard.slice(target, requiredRecipes).mkString("")
      else {
        val aScore = scoreboard(aIndex)
        val bScore = scoreboard(bIndex)
        val newRecipes = (aScore + bScore).toString.map(_.asDigit)
        val updatedScoreboard = scoreboard ++ newRecipes

        val newAIndex = (aIndex + aScore + 1) % updatedScoreboard.size
        val newBIndex = (bIndex + bScore + 1) % updatedScoreboard.size

        helper(updatedScoreboard, newAIndex, newBIndex)
      }
    }

    val result = helper(Vector(3, 7), 0, 1)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val target = input.toVector.map(_.asDigit)

    @tailrec
    def helper(scoreboard: Vector[Int], aIndex: Int, bIndex: Int): Int = {
      val aScore = scoreboard(aIndex)
      val bScore = scoreboard(bIndex)
      val newRecipes = (aScore + bScore).toString.map(_.asDigit)
      val updatedScoreboard = scoreboard ++ newRecipes

      if (updatedScoreboard.size >= target.size) {
        if (updatedScoreboard.slice(updatedScoreboard.size - target.size, updatedScoreboard.size) == target) {
          return updatedScoreboard.size - target.size
        }
        else if (newRecipes.size == 2 && updatedScoreboard.slice(updatedScoreboard.size - target.size - 1, updatedScoreboard.size - 1) == target) {
          return return updatedScoreboard.size - target.size - 1
        }
      }

      val newAIndex = (aIndex + aScore + 1) % updatedScoreboard.size
      val newBIndex = (bIndex + bScore + 1) % updatedScoreboard.size

      helper(updatedScoreboard, newAIndex, newBIndex)
    }

    val result = helper(Vector(3, 7), 0, 1)

    println(s"Result 2: $result")
  }
}

case object Day14 extends App {
  val input = Files.lines("2018/day14.txt").head
  val problem = Day14(input)
  problem.solve1()
  problem.solve2()
}
