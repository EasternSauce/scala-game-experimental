package com.easternsauce.model.creature

import cats.Monoid
import cats.data.State
import cats.implicits.catsSyntaxSemigroup
import com.easternsauce.game.ExternalEvent
import com.easternsauce.model.GameState.{GameStateTransition, getAbility, getCreature, modifyCreature}
import com.easternsauce.model.WorldDirection.WorldDirection
import com.easternsauce.model.ability.Ability
import com.easternsauce.model.ids.{AbilityId, CreatureId}
import com.easternsauce.model.{GameState, Vec2, WorldDirection}
import com.softwaremill.quicklens.ModifyPimp

import scala.language.postfixOps

trait Creature {
  val state: CreatureState

  val textureName: String
  val textureWidth: Int
  val textureHeight: Int
  val width: Float
  val height: Float
  val frameDuration: Float
  val frameCount: Int
  val neutralStanceFrame: Int
  val dirMap: Map[WorldDirection, Int]

  val speed: Float = 10f

  val abilityNames: List[String] = List()

  val defaultAbilityName = "slash"

  val abilityUsages: Map[String, AbilityUsage] = Map()

  var useAbilityTimeout: Float = 4

  implicit val id: CreatureId = state.id

  def isMoving: Boolean = state.currentSpeed > 0f

  def isPlayer: Boolean = false
  def isEnemy: Boolean = false
  def isControlledAutomatically: Boolean = false

  def facingDirection: WorldDirection = {
    state.movingDir.angleDeg() match {
      case angle if angle >= 45 && angle < 135  => WorldDirection.Up
      case angle if angle >= 135 && angle < 225 => WorldDirection.Left
      case angle if angle >= 225 && angle < 315 => WorldDirection.Down
      case _                                    => WorldDirection.Right
    }
  }

  def moveInDir(dir: Vec2)(implicit gameState: GameState): GameStateTransition = {
    State[GameState, List[ExternalEvent]] { implicit gameState =>
      (modifyCreature(_.modify(_.state.movingDir).setTo(dir)), List())
    }
  }

  def startMoving()(implicit gameState: GameState): GameStateTransition = {

    State { implicit gameState =>
      (
        modifyCreature(
          _.modify(_.state.currentSpeed).setTo(this.speed).modify(_.state.animationTimer).using(_.restart())
        ),
        List()
      )
    }
  }

  def stopMoving()(implicit gameState: GameState): GameStateTransition = {
    State { implicit gameState => (modifyCreature(_.modify(_.state.currentSpeed).setTo(0f)), List()) }
  }

  def isAlive = true // TODO

  def update(delta: Float)(implicit gameState: GameState): GameStateTransition = {
    updateTimers(delta) |+|
      (if (isControlledAutomatically) updateAutomaticControls() else Monoid[GameStateTransition].empty)
  }

  def updateTimers(delta: Float)(implicit gameState: GameState): GameStateTransition = {
    State { implicit gameState =>
      (
        modifyCreature(
          _.modifyAll(_.state.animationTimer, _.state.pathCalculationCooldownTimer, _.state.useAbilityTimer)
            .using(_.update(delta))
        ),
        List()
      )
    }

  }

  def updateAutomaticControls()(implicit gameState: GameState): GameStateTransition = Monoid[GameStateTransition].empty

  def attack(dir: Vec2)(implicit gameState: GameState): GameStateTransition = {
    implicit val abilityId: AbilityId = AbilityId.derive(id, defaultAbilityName)
    if (getCreature.abilityNames.contains(defaultAbilityName))
      getAbility.perform(dir)
    else Monoid[GameStateTransition].empty
  }

  def init()(implicit gameState: GameState): GameStateTransition = {
    State { gameState =>
      (
        // init abilities
        abilityNames.foldLeft(gameState) {
          case (gameState, abilityName) =>
            val ability = Ability.abilityByName(id, abilityName)
            gameState.modify(_.abilities).using(_.updated(ability.id, ability))
        },
        List()
      )
    }

  }

  def capability: Int = {
    if (width >= 0 && width < 2) 1
    else if (width >= 2 && width <= 4) 2
    else if (width >= 4 && width <= 6) 3
    else 4
  }

  def copy(state: CreatureState): Creature
}

case class AbilityUsage(
  weight: Float,
  minimumDistance: Float = 0f,
  maximumDistance: Float = Float.MaxValue,
  lifeThreshold: Float = 1.0f
)
