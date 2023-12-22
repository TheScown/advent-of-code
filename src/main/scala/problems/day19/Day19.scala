package space.scown.advent2023
package problems.day19

import lib.Timer.time
import lib.{Files, Problem}

import scala.annotation.tailrec

case class Day19(lines: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val blankIndex = lines.indexOf("")
    val functionLines = lines.take(blankIndex)
    val inputLines = lines.takeRight(lines.size - blankIndex - 1)

    Grammar.parseAll[Seq[Function]](Grammar.functions, functionLines.mkString("\n")) match {
      case Grammar.Success(functions, _) =>
        Grammar.parseAll[Seq[Variables]](Grammar.inputs, inputLines.mkString("")) match {
          case Grammar.Success(inputs, _) =>
            val functionMap = functions.groupBy(f => f.name).view.mapValues(x => x.head).toMap

            val result = inputs.filter(input => functionMap("in")(functionMap, input)).map(_.sum).sum

            println(s"Result 1: $result")
          case f@Grammar.Failure(msg, _) => throw new IllegalArgumentException(f.toString())
          case Grammar.Error(msg, _) => throw new IllegalArgumentException(msg)
        }
      case f@Grammar.Failure(msg, _) => throw new IllegalArgumentException(f.toString())
      case Grammar.Error(msg, _) => throw new IllegalArgumentException(msg)
    }
  }

  override def solve2(): Unit = {
    val blankIndex = lines.indexOf("")
    val functionLines = lines.take(blankIndex)

    Grammar.parseAll[Seq[Function]](Grammar.functions, functionLines.mkString("\n")) match {
      case Grammar.Success(functions, _) =>
        val functionMap = functions.groupBy(f => f.name).view.mapValues(x => x.head).toMap

        // Starting at in
        // For each function encountered
        // Run the sub functions in turn, tracking the bounds placed by previous functions
        def helper(function: Function, lowerBounds: Variables, upperBounds: Variables): BigInt = {
          println(s"${function.name}, $lowerBounds, $upperBounds")

          @tailrec
          def conditionHelper(conditionsWithBody: List[ConditionWithBody], lowerBounds: Variables, upperBounds: Variables, count: BigInt): BigInt = {
            if (conditionsWithBody.isEmpty) count
            else {
              val nextCondition = conditionsWithBody.head

              val countFromFunction: BigInt = nextCondition match {
                case ConditionWithBody(AlwaysTrue, body) =>
                  // Condition triggers – run this body with the current bounds
                  body match {
                    case Accept => (upperBounds - lowerBounds).product
                    case Reject => BigInt(0)
                    case Workflow(name) => helper(functionMap(name), lowerBounds, upperBounds)
                  }
                case ConditionWithBody(AlwaysFalse, _) =>
                  // Condition doesn't trigger (this is unreachable as AlwaysFalse is synthetic)
                  throw new IllegalStateException(s"Evaluating AlwaysFalse as a condition, ${function.name}")
                case ConditionWithBody(LessThan(variable, value), body) =>
                  if (lowerBounds(variable) >= value) {
                    // Condition doesn't trigger, proceed to next body
                    BigInt(0)
                  }
                  else if (upperBounds(variable) > value) {
                    // Condition triggers – run this body with a reduced upper bound
                    val newUpperBounds = upperBounds.withVariable(variable, value - 1)
                    body match {
                      case Accept => (newUpperBounds - lowerBounds).product
                      case Reject => BigInt(0)
                      case Workflow(name) =>
                        helper(functionMap(name), lowerBounds, newUpperBounds)
                    }
                  }
                  else {
                    // Condition triggers – run this body with the same upper bound (it's already below this condition)
                    body match {
                      case Accept => (upperBounds - lowerBounds).product
                      case Reject => BigInt(0)
                      case Workflow(name) => helper(functionMap(name), lowerBounds, upperBounds)
                    }
                  }
                case ConditionWithBody(GreaterThan(variable, value), body) =>
                  if (upperBounds(variable) <= value) {
                    // Condition doesn't trigger, proceed to next body
                    BigInt(0)
                  }
                  else if (lowerBounds(variable) < value) {
                    // Condition triggers – run this body with an increased lower bound
                    val newLowerBounds = lowerBounds.withVariable(variable, value + 1)
                    body match {
                      case Accept => (upperBounds - newLowerBounds).product
                      case Reject => BigInt(0)
                      case Workflow(name) =>
                        helper(functionMap(name), newLowerBounds, upperBounds)
                    }
                  }
                  else {
                    // Condition triggers – run this body with the same lower bound (it's already above this condition)
                    body match {
                      case Accept => (upperBounds - lowerBounds).product
                      case Reject => BigInt(0)
                      case Workflow(name) => helper(functionMap(name), lowerBounds, upperBounds)
                    }
                  }
              }

              println(s"$nextCondition, $countFromFunction")

              nextCondition.condition.complement match {
                case LessThan(variable, value) =>
                  if (upperBounds(variable) > value) {
                    conditionHelper(conditionsWithBody.tail, lowerBounds, upperBounds.withVariable(variable, value - 1), count + countFromFunction)
                  }
                  else {
                    conditionHelper(conditionsWithBody.tail, lowerBounds, upperBounds, count + countFromFunction)
                  }
                case GreaterThan(variable, value) =>
                  if (lowerBounds(variable) < value) {
                    conditionHelper(conditionsWithBody.tail, lowerBounds.withVariable(variable, value + 1), upperBounds, count + countFromFunction)
                  }
                  else {
                    conditionHelper(conditionsWithBody.tail, lowerBounds, upperBounds, count + countFromFunction)
                  }
                case _ =>
                  // AlwaysTrue or AlwaysFalse – don't change (in practice this means we've reached the default body)
                  conditionHelper(conditionsWithBody.tail, lowerBounds, upperBounds, count + countFromFunction)
              }
            }
          }

          conditionHelper(function.conditions ::: List(ConditionWithBody(AlwaysTrue, function.defaultBody)), lowerBounds, upperBounds, 0)
        }

        val result = helper(functionMap("in"), Variables(1, 1, 1, 1), Variables(4000, 4000, 4000, 4000))

        println(s"Result 2: $result")
      case f@Grammar.Failure(msg, _) => throw new IllegalArgumentException(f.toString())
      case Grammar.Error(msg, _) => throw new IllegalArgumentException(msg)
    }
  }
}

object Day19 {
  def main(args: Array[String]): Unit = {
    val value = Files.lines("day19.txt")
    Day19(value).solve1()
    time(() => Day19(value).solve2())
  }

}
