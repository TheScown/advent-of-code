package space.scown.adventofcode
package advent2020

import lib.{Files, Problem}

case class Day4(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val fullInput = input.mkString("\n")
    val passports = fullInput.split("\n\n").toVector

    val patterns = Seq(
      "byr:",
      "iyr:",
      "eyr:",
      "hgt:",
      "hcl:",
      "ecl:",
      "pid:",
    )

    val result = passports.count { passport =>
      patterns.forall(pattern => passport.contains(pattern))
    }

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val fullInput = input.mkString("\n")
    val passports = fullInput.split("\n\n").map(_.replace('\n', ' ')).toVector

    val patterns = Seq(
      "(?s).*?byr:(?:19[2-9]\\d|200[0-2])(\\D.*?|$)".r,
      "(?s).*?iyr:20(?:1\\d|20)(\\D.*?|$)".r,
      "(?s).*?eyr:20(?:2\\d|30)(\\D.*?|$)".r,
      "(?s).*?hgt:(?:1(?:[5-8]\\d|9[0-3])cm|(?:59|6\\d|7[0-6])in).*?".r,
      "(?s).*?hcl:#[0-9a-f]{6}([^0-9a-f].*?|$)".r,
      "(?s).*?ecl:(?:amb|blu|brn|gry|grn|hzl|oth).*?".r,
      "(?s).*?pid:\\d{9}(\\D.*?|$)".r,
    )

    val result = passports.count { passport =>
      val matches = patterns.map(pattern => pattern.matches(passport))
      val result = matches.forall(b => b)

      if (result) {
        println(passport)
        println(matches)
        println()
      }

      result
    }

    println(s"Result 2: $result")
  }
}

case object Day4 extends App {
  val input = Files.lines("2020/day4.txt")
  val problem = Day4(input)
  problem.solve1()
  problem.solve2()
}
