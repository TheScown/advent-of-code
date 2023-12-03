package space.scown.advent2023
package problems

import lib.{Files, Problem}

import space.scown.advent2023.problems.Day2.{bluePattern, greenPattern, redPattern}

import scala.util.matching.Regex

case class Day2(lines: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val result = parseGames()
      .filter { game => {
        !game.sets.exists {
          case BallSet(red, green, blue) => red > 12 || green > 13 || blue > 14
        }
      }}
      .map { game => game.id }
      .sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val result = parseGames()
      .map { game => {
        val maxSet = game.sets.fold(BallSet(0, 0, 0)) { (set1, set2) => {
          BallSet(
            red = Math.max(set1.red, set2.red),
            green = Math.max(set1.green, set2.green),
            blue = Math.max(set1.blue, set2.blue)
          )
        }}

        maxSet.red * maxSet.green * maxSet.blue
      }}
      .sum

    println(s"Result 2: $result")
  }

  private def parseGames() = {
    lines.map { line => {
      val parts = line.split(':')
      val id = parts(0).replace("Game ", "").toInt

      val setStrings = parts(1).split(';')

      def parseSetString(setString: String, pattern: Regex) = {
        pattern.findFirstMatchIn(setString).map(m => m.group(1)).orElse(Some("0")).map(_.toInt).get
      }

      val sets = setStrings.map { setString => {
        BallSet(
          red = parseSetString(setString, redPattern),
          green = parseSetString(setString, greenPattern),
          blue = parseSetString(setString, bluePattern)
        )
      }}.toVector

      Game(id, sets)
    }}
  }
}

case class Game(id: Int, sets: Vector[BallSet])

case class BallSet(red: Int, green: Int, blue: Int)

object Day2 {

  val redPattern: Regex = "(\\d+) red".r
  val greenPattern: Regex = "(\\d+) green".r
  val bluePattern: Regex = "(\\d+) blue".r

  def main(args: Array[String]): Unit = {
    val value = Files.lines("day2.txt")
    Day2(value).solve1()
    Day2(value).solve2()
  }

}
