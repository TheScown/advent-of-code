package space.scown.adventofcode
package advent2020

import lib.{Files, Problem}

import scala.annotation.tailrec

case class Day21(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val (mapping, ingredientSets) = parse()

    val allergenicIngredients = findAllergenicIngredients(mapping, mapping.keySet, Map()).keySet
    val result = ingredientSets.map(_ diff allergenicIngredients).map(_.size).sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val (mapping, _) = parse()

    val knownAllergens = findAllergenicIngredients(mapping, mapping.keySet, Map())

    val result = knownAllergens.toVector.sortBy(_._2).map(_._1).mkString(",")

    println(s"Result 2: $result")
  }

  @tailrec
  private def findAllergenicIngredients(
    currentMapping: Map[String, Vector[Set[String]]],
    remainingAllergens: Set[String],
    allergenIngredients: Map[String, String]
  ): Map[String, String] = {
    if (remainingAllergens.isEmpty) {
      allergenIngredients
    }
    else {
      val (updatedMapping, usedAllergens, foundAllergenicIngredients) = remainingAllergens.foldLeft((currentMapping, Set[String](), allergenIngredients)) {
        case ((mapping, usedAllergens, allergenIngredients), allergen) =>
          val possibleAllergenIngredients = mapping(allergen).reduce(_ intersect _)

          if (possibleAllergenIngredients.size == 1) {
            val allergenIngredient = possibleAllergenIngredients.head

            val updatedMapping = mapping.map { case (allergen, ingredientSets) =>
              (allergen, ingredientSets.map(_ - allergenIngredient))
            }

            (updatedMapping, usedAllergens + allergen, allergenIngredients + (allergenIngredient -> allergen))
          }
          else (mapping, usedAllergens, allergenIngredients)
      }

      findAllergenicIngredients(updatedMapping, remainingAllergens diff usedAllergens, foundAllergenicIngredients)
    }
  }

  private def parse(): (Map[String, Vector[Set[String]]], Vector[Set[String]]) = {
    val pattern = "([^(]+) \\(contains ([^)]+)\\)".r

    val parsedOutput = input.map {
      case pattern(ingredientsSsv, allergenCsv) =>
        val ingredients = ingredientsSsv.trim.split(" ").toSet
        val allergens = allergenCsv.trim.split(", ").toSet

        (ingredients, allergens)
    }

    val allAllergens = parsedOutput.map(_._2).reduce(_ union  _).toVector

    val mapping = allAllergens.map { allergen =>
      val matchingIngredientSets = parsedOutput
        .filter(_._2.contains(allergen))
        .map(_._1)

      allergen -> matchingIngredientSets
    }.toMap

    (mapping, parsedOutput.map(_._1))
  }
}

case object Day21 extends App {
  val input = Files.lines("2020/day21.txt")
  val problem = Day21(input)
  problem.solve1()
  problem.solve2()
}
