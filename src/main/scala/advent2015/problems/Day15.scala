package space.scown.adventofcode
package advent2015.problems

import lib.{Files, Problem}

case class Day15(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val ingredients = parse()

    def helper(remainingIngredients: Vector[Ingredient], remainingAmount: Int, amounts: Map[Ingredient, Long]): Long = {
      if (remainingIngredients.size == 1) {
        val finalAmounts = amounts + (remainingIngredients.head -> remainingAmount.toLong)

        finalAmounts.map {
          case (ingredient, quantity) => ingredient * quantity
        }.reduce(_ + _).score1
      }
      else {
        val nextIngredient = remainingIngredients.head

        val results = (0 to remainingAmount).map { quantity =>
          helper(remainingIngredients.tail, remainingAmount - quantity, amounts + (nextIngredient -> quantity))
        }
        results.max
      }
    }

    val result = helper(ingredients, 100, Map())

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val ingredients = parse()

    def helper(remainingIngredients: Vector[Ingredient], remainingAmount: Int, amounts: Map[Ingredient, Long]): Long = {
      if (remainingIngredients.size == 1) {
        val finalAmounts = amounts + (remainingIngredients.head -> remainingAmount.toLong)

        finalAmounts.map {
          case (ingredient, quantity) => ingredient * quantity
        }.reduce(_ + _).score2
      }
      else {
        val nextIngredient = remainingIngredients.head

        val results = (0 to remainingAmount).map { quantity =>
          helper(remainingIngredients.tail, remainingAmount - quantity, amounts + (nextIngredient -> quantity))
        }
        results.max
      }
    }

    val result = helper(ingredients, 100, Map())

    println(s"Result 2: $result")
  }


  private def parse(): Vector[Ingredient] = {
    val pattern = "([A-Z][A-Za-z]+)[^\\-0-9]+(-?\\d)[^\\-0-9]+(-?\\d)[^\\-0-9]+(-?\\d)[^\\-0-9]+(-?\\d)[^\\-0-9]+(-?\\d)".r

    input.map {
      case pattern(name, capacity, durability, flavour, texture, calories) => Ingredient(
        name = name,
        capacity = capacity.toLong,
        durability = durability.toLong,
        flavour = flavour.toLong,
        texture = texture.toLong,
        calories = calories.toLong
      )
    }

  }

  case class Ingredient(
    name: String,
    capacity: Long,
    durability: Long,
    flavour: Long,
    texture: Long,
    calories: Long
  ) {

    def *(quantity: Long): Ingredient = copy(
      capacity = capacity * quantity,
      durability = durability * quantity,
      flavour = flavour * quantity,
      texture = texture * quantity,
      calories = calories * quantity
    )

    def +(other: Ingredient): Ingredient = copy(
      capacity = capacity + other.capacity,
      durability = durability + other.durability,
      flavour = flavour + other.flavour,
      texture = texture + other.texture,
      calories = calories + other.calories
    )

    def score1: Long = {
      if (capacity <= 0 || durability <= 0 || flavour <= 0 || texture <= 0) 0
      else capacity * durability * flavour * texture
    }

    def score2: Long = {
      if (calories != 500 || capacity <= 0 || durability <= 0 || flavour <= 0 || texture <= 0) 0
      else capacity * durability * flavour * texture
    }

  }
}

case object Day15 extends App {
  val input = Files.lines("2015/day15.txt")
  val problem = Day15(input)
  problem.solve1()
  problem.solve2()
}
