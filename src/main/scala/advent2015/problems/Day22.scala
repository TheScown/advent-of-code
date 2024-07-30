package space.scown.adventofcode
package advent2015.problems

import lib.{Dijkstra, Files, Problem}

import scala.annotation.tailrec
import scala.collection.mutable

case class Day22(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val initialState = State(
      Player(50, 0, 500),
      initialEnemy,
      0,
      Seq(),
      Seq(),
      playerTurn = true
    )

    val allSpells = Seq(MagicMissile, Drain, Shield, Poison, Recharge)

    val finalState = Dijkstra.solve[State](
      initialState,
      { case (x, y) => y.manaConsumed.compareTo(x.manaConsumed) },
      _.enemy.hitPoints <= 0
    ) { state =>
      val afterEffectsEnd = startOfTurn(state)

      if (afterEffectsEnd.enemy.hitPoints <= 0) Seq(afterEffectsEnd)
      else {
        if (afterEffectsEnd.playerTurn) {
          val castableSpells = allSpells
            .filter(spell => spell.cost <= afterEffectsEnd.player.mana)
            .filter(spell => !afterEffectsEnd.effects.exists(e => e.originator == spell))

          if (castableSpells.isEmpty) Seq()
          else {
            val afterPlayerTurn = castableSpells
              .map { spell =>
                val afterCast = spell.cast(afterEffectsEnd)
                afterCast.copy(
                  player = afterCast.player.copy(mana = afterCast.player.mana - spell.cost),
                  manaConsumed = afterCast.manaConsumed + spell.cost,
                  history = afterCast.history :+ state,
                  playerTurn = false
                )
              }

            val winningStates = afterPlayerTurn.filter(s => s.enemy.hitPoints <= 0)
            if (winningStates.nonEmpty) Seq(winningStates.minBy(_.manaConsumed))
            else afterPlayerTurn
          }
        }
        else {
          val afterBossAttack = afterEffectsEnd.copy(
            player = afterEffectsEnd.player.copy(hitPoints = afterEffectsEnd.player.hitPoints - Math.max(afterEffectsEnd.enemy.damage - afterEffectsEnd.player.armour, 1))
          )

          if (afterBossAttack.player.hitPoints > 0) {
            Seq(afterBossAttack.copy(
              history = afterBossAttack.history :+ state,
              playerTurn = true
            ))
          }
          else Seq()
        }
      }
    }

    val result = finalState.manaConsumed

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    val initialState = State(
      Player(50, 0, 500),
      initialEnemy,
      0,
      Seq(),
      Seq(),
      playerTurn = true
    )

    val queue = mutable.PriorityQueue[State]() {
      case (x, y) => y.manaConsumed.compareTo(x.manaConsumed)
    }

    queue.enqueue(initialState)

    val allSpells = Seq(MagicMissile, Drain, Shield, Poison, Recharge)

    @tailrec
    def helper(): State = {
      val state = queue.dequeue()

      val afterInitialDamage = if (state.playerTurn) {
        state.copy(
          player = state.player.copy(hitPoints = state.player.hitPoints - 1)
        )
      }
      else state

      if (afterInitialDamage.player.hitPoints <= 0) helper()
      else {
        val afterEffectsEnd = startOfTurn(afterInitialDamage)

        if (afterEffectsEnd.enemy.hitPoints <= 0) afterEffectsEnd
        else {
          if (afterEffectsEnd.playerTurn) {
            val castableSpells = allSpells
              .filter(spell => spell.cost <= afterEffectsEnd.player.mana)
              .filter(spell => !afterEffectsEnd.effects.exists(e => e.originator == spell))

            if (castableSpells.isEmpty) helper()
            else {
              val afterPlayerTurn = castableSpells
                .map { spell =>
                  val afterCast = spell.cast(afterEffectsEnd)
                  afterCast.copy(
                    player = afterCast.player.copy(mana = afterCast.player.mana - spell.cost),
                    manaConsumed = afterCast.manaConsumed + spell.cost,
                    history = afterCast.history :+ state,
                    playerTurn = false
                  )
                }

              val winningStates = afterPlayerTurn.filter(s => s.enemy.hitPoints <= 0)
              if (winningStates.nonEmpty) winningStates.minBy(_.manaConsumed)
              else {
                afterPlayerTurn.foreach(s => queue.enqueue(s))
                helper()
              }
            }
          }
          else {
            val afterBossAttack = afterEffectsEnd.copy(
              player = afterEffectsEnd.player.copy(hitPoints = afterEffectsEnd.player.hitPoints - Math.max(afterEffectsEnd.enemy.damage - afterEffectsEnd.player.armour, 1))
            )

            if (afterBossAttack.player.hitPoints > 0) {
              queue.enqueue(afterBossAttack.copy(
                history = afterBossAttack.history :+ state,
                playerTurn = true
              ))
            }

            helper()
          }
        }
      }
    }

    val finalState = helper()
    val result = finalState.manaConsumed

    println(s"Result 2: $result")
  }

  private def startOfTurn(state: State) = {
    val afterEffectsApplied = state.effects.foldLeft(state)((state, effect) => effect.continue(state))
    val (remainingEffects, deadEffects) = state.effects
      .map(e => e.copy(timeRemaining = e.timeRemaining - 1))
      .partition(e => e.timeRemaining > 0)

    val afterEffectsEnd = deadEffects.foldLeft(afterEffectsApplied)((state, effect) => effect.end(state))
    afterEffectsEnd.copy(
      effects = remainingEffects
    )
  }

  private lazy val initialEnemy: Enemy = {
    val pattern = "\\d+".r
    val values = input.map(line => pattern.findFirstIn(line).get.toInt)
    Enemy(values(0), values(1))
  }

  case class Player(hitPoints: Int, armour: Int, mana: Int)

  case class Enemy(hitPoints: Int, damage: Int)

  case class State(player: Player, enemy: Enemy, manaConsumed: Int, effects: Seq[Effect], history: Seq[State], playerTurn: Boolean) {
    override def equals(obj: Any): Boolean = obj match {
      case State(player, enemy, manaConsumed, effects, _, playerTurn) => player == this.player && enemy == this.enemy && manaConsumed == this.manaConsumed && effects == this.effects && playerTurn == this.playerTurn
      case _ => false
    }

    override def hashCode(): Int = player.hashCode() + 37 * (enemy.hashCode() + 37 * (manaConsumed + 37 * effects.hashCode() + playerTurn.hashCode()))
  }

  sealed trait Spell {
    def cost: Int

    def cast(state: State): State
  }

  case object MagicMissile extends Spell {
    override def cost: Int = 53

    override def cast(state: State): State = state.copy(
      enemy = state.enemy.copy(
        hitPoints = state.enemy.hitPoints - 4
      )
    )
  }

  case object Drain extends Spell {

    override def cost: Int = 73

    override def cast(state: State): State = state.copy(
      player = state.player.copy(
        hitPoints = state.player.hitPoints + 2
      ),
      enemy = state.enemy.copy(
        hitPoints = state.enemy.hitPoints - 2
      )
    )
  }

  case object Shield extends Spell {
    override def cost: Int = 113

    override def cast(state: State): State = state.copy(
      player = state.player.copy(armour = state.player.armour + 7),
      effects = state.effects :+ Effect(
        timeRemaining = 6,
        originator = Shield,
        continue = state => state,
        end = state => state.copy(
          player = state.player.copy(armour = state.player.armour - 7)
        )
      )
    )
  }

  case object Poison extends Spell {
    override def cost: Int = 173

    override def cast(state: State): State = state.copy(
      effects = state.effects :+ Effect(
        timeRemaining = 6,
        originator = Poison,
        continue = state => state.copy(
          enemy = state.enemy.copy(hitPoints = state.enemy.hitPoints - 3)
        ),
        end = state => state
      )
    )
  }

  case object Recharge extends Spell {
    override def cost: Int = 229

    override def cast(state: State): State = state.copy(
      effects = state.effects :+ Effect(
        timeRemaining = 5,
        originator = Recharge,
        continue = state => state.copy(
          player = state.player.copy(mana = state.player.mana + 101)
        ),
        end = state => state
      )
    )
  }

  case class Effect(
    timeRemaining: Int,
    originator: Spell,
    continue: State => State,
    end: State => State,
  )
}

case object Day22 extends App {
  val input = Files.lines("2015/day22.txt")
  val problem = Day22(input)
  problem.solve1()
  problem.solve2()
}
