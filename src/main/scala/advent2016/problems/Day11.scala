package space.scown.adventofcode
package advent2016.problems

import lib.{BFS, Files, Problem, Timer}

case class Day11(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val floors = parse()

    val result = solve(floors)

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val floors = parse()
    val updatedFloors = floors.updated(0, Floor(floors(0).things ++ Set(
      Thing("elerium", "generator"),
      Thing("elerium", "microchip"),
      Thing("dilithium", "generator"),
      Thing("dilithium", "microchip")
    )))

    val result = solve(updatedFloors)

    println(s"Result 2: $result")
  }

  private def solve(floors: Vector[Floor]): Int = {
    val initialState = State(0, floors, 0)
    val result = BFS.solve[State](initialState, s => s.isComplete) {
      case State(elevatorFloor, floors, moves) =>
        val minFloor = floors.indexWhere(f => f.things.nonEmpty)
        val nextFloors = Seq(elevatorFloor - 1, elevatorFloor + 1).filter(f => f >= minFloor && f < floors.size)
        val newStates = nextFloors.flatMap { nextFloorId =>
          val currentFloor = floors(elevatorFloor)
          val nextFloor = floors(nextFloorId)
          val currentFloorThings = currentFloor.things.toVector

          val possibleMoves = currentFloorThings.combinations(2).toVector ++ currentFloorThings.combinations(1)
          possibleMoves.map { move =>
              val newCurrentFloor = currentFloor.copy(things = (currentFloorThings diff move).toSet)
              val newNextFloor = nextFloor.copy(things = nextFloor.things ++ move)
              State(
                nextFloorId,
                floors
                  .updated(elevatorFloor, newCurrentFloor)
                  .updated(nextFloorId, newNextFloor),
                moves + 1
              )
            }
            .filter(state => state.floors.forall(_.isValid))
        }

        newStates
    }
    result.get.moves
  }

  private def parse(): Vector[Floor] = {
    val prefix = "^The [a-z]+ floor contains ".r
    input.map { line =>
      val preProcessed = prefix.replaceFirstIn(line, "").replace(".", "")
      preProcessed match {
        case "nothing relevant" => Floor()
        case _ =>
          val things = preProcessed.split(", (?:and )?")
            .map(_.replace("a ", ""))
            .map { s =>
              val parts = s.split(" ")
              val purpose = parts(1)
              val element = parts(0).replace("-compatible", "")
              Thing(element, purpose)
            }
            .toSet

          Floor(things)
      }
    }
  }

  case class State(elevatorFloor: Int, floors: Vector[Floor], moves: Int) {
    def isComplete: Boolean = {
      val reversed = floors.reverse
      reversed.tail.forall(f => f.things.isEmpty)
    }

    override def equals(obj: Any): Boolean = {
      if (!obj.isInstanceOf[State]) false
      else {
        val other = obj.asInstanceOf[State]

        if (other.elevatorFloor != elevatorFloor) false
        else other.floors == floors
      }
    }

    override def hashCode(): Int = 37 * elevatorFloor + floors.hashCode()
  }

  case class Floor(things: Set[Thing] = Set()) {
    def isValid: Boolean = {
      val generators = things.filter(_.purpose == "generator")

      if (generators.isEmpty) true
      else things.filter(_.purpose == "microchip")
        .forall(t => things.exists(s => s.element == t.element && s.purpose == "generator"))
    }
  }

  case class Thing(element: String, purpose: String)
}

case object Day11 extends App {
  val input = Files.lines("2016/day11.txt")
  val problem = Day11(input)
  Timer.time(() => problem.solve1())
  Timer.time(() => problem.solve2())
}