package space.scown.adventofcode
package advent2018.problems

import lib.{Files, Integers, Problem, Timer}

import scala.annotation.tailrec
import scala.language.postfixOps
import scala.math.Numeric.IntIsIntegral
import scala.util.parsing.combinator.RegexParsers

case class Day24(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val groups = parse()

    val result = fight(groups)._1

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val groups = parse()

    val result = Integers.naturalNumbers(IntIsIntegral).map { boost =>
      val boostedGroups = groups.map { g =>
        if (g.side == Infection) g
        else g.copy(attackDamage = g.attackDamage + boost)
      }

      fight(boostedGroups)
    }.find(_._2 == ImmuneSystem).get._1

    println(s"Result 2: $result")
  }

  @tailrec
  private def fight(groups: Vector[Group]): (Int, Side) = {
    val (infectionUnits, immuneUnits) = groups.partition(_.side == Infection)

    if (infectionUnits.isEmpty)
      (immuneUnits.map(_.units).sum, ImmuneSystem)
    else if (immuneUnits.isEmpty)
      (infectionUnits.map(_.units).sum, Infection)
    else {
      val (targets, _, _) = groups.sorted(Group.byEffectivePowerThenInitiative).foldLeft(
        (Map[Group, Group](), infectionUnits, immuneUnits)
      ) { case ((targets, remainingInfection, remainingImmune), group) =>
        val chooseFrom = if (group.side == Infection) remainingImmune else remainingInfection

        if (chooseFrom.isEmpty) (targets, remainingInfection, remainingImmune)
        else {
          val sortedTargets = chooseFrom.sorted(group.targetOrder)
          val chosenTarget = sortedTargets.head

          if (group.damageDealtTo(chosenTarget) > 0) {
            val updatedTargets = targets + (group -> chosenTarget)
            if (group.side == Infection) (updatedTargets, remainingInfection, sortedTargets.tail)
            else (updatedTargets, sortedTargets.tail, remainingImmune)
          }
          else {
            (targets, remainingInfection, remainingImmune)
          }
        }
      }

      @tailrec
      def performAttacks(remainingAttackers: Vector[Group], updatedGroups: Map[Group, Group]): Map[Group, Group] = {
        if (remainingAttackers.isEmpty) updatedGroups
        else {
          val nextAttacker = remainingAttackers.head

          if (!targets.contains(nextAttacker)) performAttacks(remainingAttackers.tail, updatedGroups)
          else {
            val target = targets(nextAttacker)
            val updatedNextAttacker = updatedGroups.getOrElse(nextAttacker, nextAttacker)
            val attackedTarget = updatedNextAttacker.attack(target)

            performAttacks(remainingAttackers.tail, updatedGroups + (target -> attackedTarget))
          }
        }
      }

      val afterAttacks = performAttacks(groups.sorted(Group.byInitiative), Map())
      val updatedGroups = groups.map(g => afterAttacks.getOrElse(g, g)).filterNot(_.units == 0)

      // If nothing was achieved, we've hit a stalemate which is a default infection win
      if (updatedGroups == groups) (-1, Infection)
      else fight(updatedGroups)
    }
  }

  private def parse(): Vector[Group] = {
    Grammar.parseAll[List[Army]](Grammar.armies, input.mkString("\n")) match {
      case Grammar.Success(armies, _) => armies.foldLeft(Vector[(Group, Side)]()) { (acc, army) =>
        acc ++ army.groups.map(g => (g, army.side))
      }.map { case (group, side) => group.copy(side = side) }
      case Grammar.NoSuccess.I(msg, next) =>
        println(next.pos)
        throw new IllegalArgumentException(msg)
    }
  }

  private case object Grammar extends RegexParsers {
    def armies: Parser[List[Army]] = army+
    def army: Parser[Army] = (infection | immuneSystem) ~ (":" ~> rep(unit)) ^^ {
      case side ~ units => Army(units.toVector, side)
    }
    def immuneSystem: Parser[Side] = "Immune System" ^^ (_ => ImmuneSystem)
    def infection: Parser[Side] = "Infection" ^^ (_ => Infection)
    def unit: Parser[Group] = number ~
      ("units each with" ~> number) ~
      ("hit points" ~> opt("(" ~> multipliers <~ ")")) ~
      ("with an attack that does" ~> number) ~
      element ~
      ("damage at initiative" ~> number) ^^ {
      case units ~ hitPoints ~ multipliers ~ attackDamage ~ attackType ~ initiative => Group(
        units, hitPoints, multipliers.getOrElse(Map()), attackType, attackDamage, initiative
      )
    }
    def multipliers: Parser[Map[Multiplier, Set[String]]] = repsep(weakTo | immuneTo, "; ") ^^ (_.toMap)
    def weakTo: Parser[(Multiplier, Set[String])] = "weak to" ~> repsep(element, ", ") ^^ (s => (Weak, s.toSet))
    def immuneTo: Parser[(Multiplier, Set[String])] = "immune to" ~> repsep(element, ", ") ^^ (s => (Immune, s.toSet))
    def element: Parser[String] = "[a-z]+".r
    def number: Parser[Int] = "\\d+".r ^^ (_.toInt)
  }

  private case class Army(groups: Vector[Group], side: Side)

  private case class Group(
    units: Int,
    hitPoints: Int,
    multipliers: Map[Multiplier, Set[String]],
    attackType: String,
    attackDamage: Int,
    initiative: Int,
    side: Side = Unassigned,
  ) {
    val effectivePower: Int = units * attackDamage

    def damageDealtTo(other: Group): Int = {
      if (other.multipliers.getOrElse(Immune, Set()).contains(attackType)) 0
      else if (other.multipliers.getOrElse(Weak, Set()).contains(attackType)) 2 * effectivePower
      else effectivePower
    }

    def targetOrder: Ordering[Group] =
      Ordering.by[Group, (Int, Int, Int)](g => (damageDealtTo(g), g.effectivePower, g.initiative)).reverse

    def attack(target: Group): Group = {
      val damage = damageDealtTo(target)
      val unitsLost = math.min(damage / target.hitPoints, target.units)
      target.copy(units = target.units - unitsLost)
    }
  }

  private case object Group {
    val byEffectivePowerThenInitiative: Ordering[Group] =
      Ordering.by[Group, (Int, Int)](g => (g.effectivePower, g.initiative)).reverse

    val byInitiative: Ordering[Group] = Ordering.by[Group, Int](_.initiative).reverse
  }

  private trait Side
  private case object ImmuneSystem extends Side
  private case object Infection extends Side
  private case object Unassigned extends Side

  private trait Multiplier
  private case object Weak extends Multiplier
  private case object Immune extends Multiplier
}

case object Day24 extends App {
  val input = Files.lines("2018/day24.txt")
  val problem = Day24(input)
  Timer.time(() => problem.solve1())
  Timer.time(() => problem.solve2())
}
