package space.scown.adventofcode
package advent2018.problems

import lib._

import scala.annotation.tailrec

case class Day15(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val (grid, units) = parse()

    val result = simulate(grid, units, 3, failOnElfDeath = false).get

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val (grid, units) = parse()

    val result = Integers.naturalNumbers[Int].drop(3).map(simulate(grid, units, _, failOnElfDeath = true)).find(_.isDefined).get.get

    // 32045 (8 attack) = too low, 33698 (8 attack) = wrong,
    println(s"Result 2: $result")
  }

  private def simulate(grid: Grid[Boolean], units: Vector[Combatant], elfAttackPower: Int, failOnElfDeath: Boolean): Option[Int] = {
    @tailrec
    def helper(remainingUnits: Vector[Combatant], updatedUnits: Vector[Combatant], completedRounds: Int): Option[Int] = {
      if (remainingUnits.isEmpty) {
        val sortedUpdatedUnits = updatedUnits.sortBy(_.position)(Grid.ordering)
//        val charGrid = grid.map(x => if (x) "#" else ".")
//        val gridWithUnits = sortedUpdatedUnits.foldLeft(charGrid){ (grid, u) =>
//          grid.updated(u.position, if (u.unitType == Elf) s"E(${u.hitPoints})" else s"G(${u.hitPoints})")
//        }
//
//        val withInfo = gridWithUnits.values.map { v =>
//          val unitString = v.filter(_.contains('(')).mkString(", ")
//          v.map(s => s.replaceAll("\\([^)]*\\)", "")).mkString("") + (if (unitString.nonEmpty) "  " + unitString else "")
//        }
//        println(s"Round: ${completedRounds + 1}")
//        withInfo.foreach(println)
//        println()

        helper(sortedUpdatedUnits, Vector(), completedRounds + 1)
      } else {
        val currentUnit = remainingUnits.head

        val position = currentUnit.position
        val otherUnits = remainingUnits.tail ++ updatedUnits

        val possibleTargets = otherUnits
          .filter(u => u.unitType != currentUnit.unitType)

        if (possibleTargets.isEmpty) {
          if (failOnElfDeath && !otherUnits.exists(_.unitType == Elf) && currentUnit.unitType != Elf) None
          else Some(completedRounds * (otherUnits.map(_.hitPoints).sum + currentUnit.hitPoints))
        }
        else {
          val neighbours = grid.neighbours(position).filter(!grid.apply(_))
          val adjacentUnits = possibleTargets.filter(u => neighbours.contains(u.position))

          val unitAfterMove = if (adjacentUnits.nonEmpty) {
            currentUnit
          }
          else {
            val possibleDestinations = possibleTargets.flatMap{ u =>
              grid.neighbours(u.position).filter(p => !grid.apply(p))
            }.toSet
            val reachableSquares = DFS.reachable(position) { p =>
              grid.neighbours(p).filter(p => !grid(p) && !otherUnits.map(_.position).contains(p))
            }
            val possibleReachableDestinations = reachableSquares intersect possibleDestinations

            if (possibleReachableDestinations.isEmpty) currentUnit
            else {
              val distancesToDestinations = neighbours.filterNot(otherUnits.map(_.position).contains).flatMap { n =>
                val distances = possibleReachableDestinations.map { p =>
                  (n, p, BFS.solve[PathState](PathState(n, 0), _.position == p) { state =>
                    grid.neighbours(state.position)
                      .filter(p => !grid.apply(p) && !otherUnits.map(_.position).contains(p))
                      .map(PathState(_, state.moves + 1))
                  }.get.moves + 1)
                }

                distances.toVector
              }
              val sortedDestinations = distancesToDestinations.sorted[(Complex[Int], Complex[Int], Int)]{ case ((n1, p1, d1), (n2, p2, d2)) =>
                if (d1 < d2) -1
                else if (d1 > d2) 1
                else {
                  if (Grid.ordering.lt(p1, p2)) -1
                  else if (Grid.ordering.gt(p1, p2)) 1
                  else Grid.ordering.compare(n1, n2)
                }
              }
              currentUnit.copy(position = sortedDestinations.head._1)
            }
          }

          val neighboursAfterMove = grid.neighbours(unitAfterMove.position).filter(!grid.apply(_))
          val attackTargets = possibleTargets.filter(u => neighboursAfterMove.contains(u.position))

          if (attackTargets.nonEmpty) {
            val toHit = attackTargets.sortBy(_.position)(Grid.ordering).minBy(_.hitPoints)
            val afterAttack = toHit.copy(hitPoints = toHit.hitPoints - currentUnit.attackPower)

            if (afterAttack.hitPoints <= 0) {
              if (afterAttack.unitType == Elf && failOnElfDeath) None
              else helper(remainingUnits.tail.filterNot(_.id == afterAttack.id), updatedUnits.filterNot(_.id == afterAttack.id) :+ unitAfterMove, completedRounds)
            }
            else if (remainingUnits.tail.exists(_.id == afterAttack.id)) {
              val index = remainingUnits.tail.indexWhere(_.id == afterAttack.id)
              helper(remainingUnits.tail.updated(index, afterAttack), updatedUnits :+ unitAfterMove, completedRounds)
            }
            else {
              val index = updatedUnits.indexWhere(_.id == afterAttack.id)
              helper(remainingUnits.tail, updatedUnits.updated(index, afterAttack) :+ unitAfterMove, completedRounds)
            }
          }
          else {
            helper(remainingUnits.tail, updatedUnits :+ unitAfterMove, completedRounds)
          }
        }
      }
    }

    val unitsWithCorrectAttack = units.map {
      case u@Combatant(Elf, _, _, _, _) => u.copy(attackPower = elfAttackPower)
      case u@Combatant(Goblin, _, _, _, _) => u.copy(attackPower = 3)
    }

    helper(unitsWithCorrectAttack.sortBy(_.position)(Grid.ordering), Vector(), 0)
  }

  def parse(): (Grid[Boolean], Vector[Combatant]) = {
    val initialGrid = Grid(input.map(_.toVector))

    val units = initialGrid.zipWithIndex
      .filter { case (c, _) => c == 'E' || c == 'G' }
      .map { case (c, p) => new Combatant(c, p) }

    (initialGrid.map(_ == '#'), units.toVector)
  }

  sealed trait UnitType

  private case object Elf extends UnitType

  private case object Goblin extends UnitType

  case class Combatant(
    unitType: UnitType,
    hitPoints: Int,
    position: Complex[Int],
    id: Complex[Int],
    attackPower: Int = 3
  ) {
    def this(unitType: Char, position: Complex[Int]) = {
      this(if (unitType == 'E') Elf else Goblin, 200, position, position)
    }
  }

  private case class PathState(position: Complex[Int], moves: Int) {
    override def hashCode(): Int = position.hashCode()

    override def equals(obj: Any): Boolean = obj match {
      case PathState(p, _) => p == position
      case _ => false
    }
  }
}

case object Day15 extends App {
  val input = Files.lines("2018/day15.txt")
  val problem = Day15(input)
  Timer.time(() => problem.solve1())
  Timer.time(() => problem.solve2())
}
