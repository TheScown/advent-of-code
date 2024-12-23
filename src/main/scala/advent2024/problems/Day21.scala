package space.scown.adventofcode
package advent2024.problems

import lib.{BFS, Complex, Files, Grid, Problem, Timer}

import scala.annotation.tailrec

case class Day21(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val keypad = Grid(Vector(
      Vector(7, 8, 9),
      Vector(4, 5, 6),
      Vector(1, 2, 3),
      Vector(-1, 0, 0xA),
    ))
    val keypadIndices = keypad.indices.toSet

    val aButton = Complex(0xA, 0)

    val remote = Grid(Vector(
      Vector(Complex.ZERO[Int], Complex.I[Int], aButton),
      Vector(-Complex.ONE[Int], -Complex.I[Int], Complex.ONE[Int]),
    ))
    val remoteIndices = remote.indices.toSet

    val remoteAButtonPosition = remote.indexOf(aButton).get
    val initialState = ArmState(remoteAButtonPosition, Vector.fill(1)(remoteAButtonPosition), keypad.indexOf(0xA).get, "")
    val goal = (code: String) => (state: ArmState) => state.codeTyped == code

    val result = input.map { code =>
      val finalState = BFS.solve(initialState, goal(code)) { (state, steps) =>
        state match {
          case ArmState(arm3Position, intermediateArmPositions, arm1Position, codeTyped) =>
            // new states:
            // move arm3 to his non null neighbours
            // press A, which presses the button arm3 is pointing at, move arm 2 accordingly
            // if arm2 is pointing at A, move arm1 accordingly
            // if arm1 is pointing at A, enter the corresponding digit
            // if we type the wrong digit, there are no new states
            @tailrec
            def pressButton(previousArmButton: Complex[Int], remainingArms: Vector[Complex[Int]]): Seq[ArmState] = {
              if (remainingArms.isEmpty) {
                // We've reached the bottom, poke arm1
                if (previousArmButton != aButton) {
                  val newArm1Position = arm1Position + previousArmButton
                  if (keypadIndices.contains(newArm1Position) && keypad(newArm1Position) != -1) Seq(state.copy(arm1Position = newArm1Position))
                  else Seq()
                }
                else {
                  // Arm 2 presses A, so arm1 presses his button
                  val arm1Button = keypad(arm1Position)
                  val newCode = codeTyped + arm1Button.toHexString.toUpperCase

                  if (newCode == code.substring(0, newCode.length)) Seq(state.copy(codeTyped = newCode))
                  else Seq()
                }
              }
              else {
                // We're one of the higher arms
                if (previousArmButton != aButton) {
                  val nextArmPosition = remainingArms.head + previousArmButton
                  if (remoteIndices.contains(nextArmPosition) && remote(nextArmPosition) != Complex.ZERO[Int]) Seq(state.copy(intermediateArmPositions = intermediateArmPositions.updated(intermediateArmPositions.size - remainingArms.size, nextArmPosition)))
                  else Seq()
                }
                else {
                  val nextArmButton = remote(remainingArms.head)
                  pressButton(nextArmButton, remainingArms.tail)
                }
              }
            }

            val arm3Moves = remote.neighbours(arm3Position)
              .filter(n => remote(n) != Complex.ZERO[Int])
              .map(p => state.copy(arm3Position = p))

            val arm3Button = remote(arm3Position)

            arm3Moves ++ pressButton(arm3Button, intermediateArmPositions)
        }
      }

      val numeric = code.substring(0, 3).toInt
      numeric * finalState.get.steps
    }.sum

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val keypad = Grid(Vector(
      Vector(7, 8, 9),
      Vector(4, 5, 6),
      Vector(1, 2, 3),
      Vector(-1, 0, 0xA),
    ))
    val keypadIndices = keypad.indices.toSet

    val aButton = Complex(0xA, 0)

    val remote = Grid(Vector(
      Vector(Complex.ZERO[Int], Complex.I[Int], aButton),
      Vector(-Complex.ONE[Int], -Complex.I[Int], Complex.ONE[Int]),
    ))
    val remoteIndices = remote.indices.toSet

    val remoteAButtonPosition = remote.indexOf(aButton).get

    (1 to 6).foreach { i =>
      val initialState = ArmState(remoteAButtonPosition, Vector.fill(i)(remoteAButtonPosition), keypad.indexOf(0xA).get, "")
      val goal = (code: String) => (state: ArmState) => state.codeTyped == code

      val result = input.map { code =>
        val finalState = BFS.solve(initialState, goal(code)) { (state, _) =>
          state match {
            case ArmState(arm3Position, intermediateArmPositions, arm1Position, codeTyped) =>
              // new states:
              // move arm3 to his non null neighbours
              // press A, which presses the button arm3 is pointing at, move arm 2 accordingly
              // if arm2 is pointing at A, move arm1 accordingly
              // if arm1 is pointing at A, enter the corresponding digit
              // if we type the wrong digit, there are no new states
              @tailrec
              def pressButton(previousArmButton: Complex[Int], remainingArms: Vector[Complex[Int]]): Seq[ArmState] = {
                if (remainingArms.isEmpty) {
                  // We've reached the bottom, poke arm1
                  if (previousArmButton != aButton) {
                    val newArm1Position = arm1Position + previousArmButton
                    if (keypadIndices.contains(newArm1Position) && keypad(newArm1Position) != -1) Seq(state.copy(arm1Position = newArm1Position))
                    else Seq()
                  }
                  else {
                    // Arm 2 presses A, so arm1 presses his button
                    val arm1Button = keypad(arm1Position)
                    val newCode = codeTyped + arm1Button.toHexString.toUpperCase

                    if (newCode == code.substring(0, newCode.length)) Seq(state.copy(codeTyped = newCode))
                    else Seq()
                  }
                }
                else {
                  // We're one of the higher arms
                  if (previousArmButton != aButton) {
                    val nextArmPosition = remainingArms.head + previousArmButton
                    if (remoteIndices.contains(nextArmPosition) && remote(nextArmPosition) != Complex.ZERO[Int]) Seq(state.copy(intermediateArmPositions = intermediateArmPositions.updated(intermediateArmPositions.size - remainingArms.size, nextArmPosition)))
                    else Seq()
                  }
                  else {
                    val nextArmButton = remote(remainingArms.head)
                    pressButton(nextArmButton, remainingArms.tail)
                  }
                }
              }

              val arm3Moves = remote.neighbours(arm3Position)
                .filter(n => remote(n) != Complex.ZERO[Int])
                .map(p => state.copy(arm3Position = p))

              val arm3Button = remote(arm3Position)

              arm3Moves ++ pressButton(arm3Button, intermediateArmPositions)
          }
        }

        println(code, i, finalState.get.steps)

        val numeric = code.substring(0, 3).toInt
        numeric * finalState.get.steps
      }.sum

      println(s"Result 2: $result")
    }
  }

  private def getPaths[T](grid: Grid[T], nullValue: T) = {
    grid.zipWithIndex.filter(_._1 != nullValue).map { case (value, address) =>
      val pathsFromHere = BFS.reachable(PathState(address, Vector())) {
        case p@PathState(position, pathSoFar) =>
          println(p)
          if (grid(position) == nullValue) Seq()
          else {
            val neighbours = grid.neighbours(position)
            neighbours.map(n => PathState(n, pathSoFar :+ (n - position)))
          }
      }

      println(address, value, pathsFromHere)

      val pathsFromHereMap = pathsFromHere.map { p =>
        p.value match {
          case PathState(destination, path) =>
            grid(destination) -> path
        }
      }.toMap

      value -> pathsFromHereMap
    }.toMap
  }

  private def codeToKeypad(code: String): Vector[Int] = {
    code.map {
      case 'A' => 0xA
      case c => c.asDigit
    }.toVector
  }

  case class ArmState(arm3Position: Complex[Int], intermediateArmPositions: Vector[Complex[Int]], arm1Position: Complex[Int], codeTyped: String)

  case class PathState(position: Complex[Int], history: Vector[Complex[Int]]) {
    override def hashCode(): Int = position.hashCode()

    override def equals(obj: Any): Boolean = obj match {
      case PathState(p, _) => p == position
      case _ => false
    }
  }
}

case object Day21 extends App {
  val input = Files.lines("2024/day21.txt")
  val problem = Day21(input)
  problem.solve1()
  Timer.time(() => problem.solve2())
}
