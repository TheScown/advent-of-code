package space.scown.adventofcode
package advent2015.problems

import lib.{Files, Problem}

case class Day21(input: Vector[String]) extends Problem {

  override def solve1(): Unit = {
    val shop = parseShop()

    val players = for {
      weapon <- shop.weapons
      armour <- shop.armour.combinations(0) ++ shop.armour.combinations(1)
      rings <- shop.rings.combinations(0) ++ shop.rings.combinations(1) ++ shop.rings.combinations(2)
    } yield Player(Set(weapon) ++ armour.toSet ++ rings.toSet)

    val sortedPlayers = players.sortBy(_.cost)

    val cheapestWinner = sortedPlayers.find { p =>
      val playerTurns = Math.ceil(enemy.hitPoints.toDouble / Math.max(p.damage - enemy.armour, 1).toDouble)
      val enemyTurns = Math.ceil(p.hitPoints.toDouble / Math.max(enemy.damage - p.armour, 1).toDouble)

      playerTurns <= enemyTurns
    }.get

    val result = cheapestWinner.cost

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val shop = parseShop()

    val players = for {
      weapon <- shop.weapons
      armour <- shop.armour.combinations(0) ++ shop.armour.combinations(1)
      rings <- shop.rings.combinations(0) ++ shop.rings.combinations(1) ++ shop.rings.combinations(2)
    } yield Player(Set(weapon) ++ armour.toSet ++ rings.toSet)

    val sortedPlayers = players.sortBy(-_.cost)

    val mostExpensiveLoser = sortedPlayers.find { p =>
      val playerTurns = Math.ceil(enemy.hitPoints.toDouble / Math.max(p.damage - enemy.armour, 1).toDouble)
      val enemyTurns = Math.ceil(p.hitPoints.toDouble / Math.max(enemy.damage - p.armour, 1).toDouble)

      playerTurns > enemyTurns
    }.get

    val result = mostExpensiveLoser.cost

    println(s"Result 2: $result")
  }

  private def parseShop(): Shop = {
    val lines = Files.lines("2015/day21shop.txt")
    val (weaponLines, armourAndRings) = lines.span(_.nonEmpty)
    val (armourLines, ringLines) = armourAndRings.tail.span(_.nonEmpty)

    val pattern = "([A-Za-z +123]+)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)".r

    val itemSets = Seq(weaponLines, armourLines, ringLines.tail).map { lines =>
      lines.map {
        case pattern(name, cost, damage, armour) => Item(name, cost.toInt, damage.toInt, armour.toInt)
      }
    }

    Shop(itemSets.head, itemSets(1), itemSets(2))
  }

  private lazy val enemy: Enemy = {
    val pattern = "\\d+".r
    val values = input.map(line => pattern.findFirstIn(line).get.toInt)
    Enemy(values(0), values(1), values(2))
  }

  private case class Enemy(hitPoints: Int, damage: Int, armour: Int)
  private case class Item(name: String, cost: Int, damage: Int, armour: Int)
  private case class Shop(weapons: Seq[Item], armour: Seq[Item], rings: Seq[Item])
  private case class Player(items: Set[Item]) {
    val cost: Int = items.toSeq.map(_.cost).sum
    val damage: Int = items.toSeq.map(_.damage).sum
    val armour: Int = items.toSeq.map(_.armour).sum
    val hitPoints = 100
  }

}

case object Day21 extends App {
  val input = Files.lines("2015/day21.txt")
  val problem = Day21(input)
  problem.solve1()
  problem.solve2()
}
