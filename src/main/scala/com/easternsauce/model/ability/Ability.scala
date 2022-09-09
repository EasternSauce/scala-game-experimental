package com.easternsauce.model.ability

import cats.data.State
import cats.implicits.catsSyntaxSemigroup
import cats.kernel.Monoid
import com.badlogic.gdx.math.Vector2
import com.easternsauce.game.{AbilityBodyActivateEvent, AbilityBodyDestroyEvent, Constants, ExternalEvent}
import com.easternsauce.model.GameState.{GameStateTransition, getAbility, getCreature, modifyAbility, modifyCreature}
import com.easternsauce.model.ids.{AbilityId, CreatureId}
import com.easternsauce.model.{GameState, Vec2}
import com.softwaremill.quicklens._

import scala.util.chaining.scalaUtilChainingOps

trait Ability {
  val state: AbilityState
  val cooldownTime: Float

  val textureWidth: Int
  val textureHeight: Int
  val activeTime: Float
  val channelTime: Float
  val channelSpriteType: String
  val activeSpriteType: String
  val channelFrameCount: Int
  val activeFrameCount: Int
  val channelFrameDuration: Float
  val activeFrameDuration: Float
  val scale: Float = 1.0f
  val initSpeed: Float = 0f
  val activeAnimationLooping: Boolean = false
  val channelAnimationLooping: Boolean = false

  val attackRange: Float = 1.8f

  implicit def id: AbilityId = state.id
  implicit def creatureId: CreatureId = state.creatureId

  def width: Float = textureWidth.toFloat * scale / Constants.PPM
  def height: Float = textureHeight.toFloat * scale / Constants.PPM

  def ableToPerform(implicit gameState: GameState): Boolean =
    getCreature.isAlive && !state.justPerformed && state.stage == AbilityStage.Inactive && state.stageTimer.time > cooldownTime && getCreature.state.stamina > 0 && !onCooldown

  def onCooldown: Boolean = false // TODO

  def perform(dir: Vec2)(implicit gameState: GameState): GameStateTransition =
    if (ableToPerform)
      State[GameState, List[ExternalEvent]] { implicit gameState =>
        (
          gameState.pipe(
            implicit gameState =>
              modifyAbility(
                _.modify(_.state.dirVector)
                  .setTo(Some(dir))
              )
          ),
          List()
        )
      } |+|
        State[GameState, List[ExternalEvent]] { implicit gameState =>
          (modifyCreature(_.modify(_.state.actionDirVector).setTo(dir)), List())
        } |+|
        State { implicit gameState =>
          (
            modifyAbility(
              _.modify(_.state.justPerformed)
                .setTo(true)
                .modify(_.state.dirVector)
                .setTo(Some(dir))
            ),
            List()
          )
        } |+|
        State { implicit gameState =>
          (
            modifyCreature(
              _.modify(_.state.staminaRegenerationDisabledTimer)
                .using(_.restart())
                .modify(_.state.isStaminaRegenerationDisabled)
                .setTo(true)
            ),
            List()
          )
        } |+|
        getCreature.takeStaminaDamage(15f)
    else Monoid[GameStateTransition].empty

  def onActiveStart()(implicit gameState: GameState): GameStateTransition

  def onActiveUpdate()(implicit gameState: GameState): GameStateTransition

  def onChannelStart()(implicit gameState: GameState): GameStateTransition

  def onChannelUpdate()(implicit gameState: GameState): GameStateTransition

  def onInactiveStart()(implicit gameState: GameState): GameStateTransition

  def updateHitbox()(implicit gameState: GameState): GameStateTransition = {
    if (state.dirVector.isEmpty)
      throw new RuntimeException("trying to update hitbox without setting dir vector!")

    val dirVector = state.dirVector.get match {
      case dirVector if dirVector.length <= 0 => Vec2(1, 0).normal
      case dirVector                          => dirVector
    }

    val creature = getCreature

    val theta = new Vector2(dirVector.x, dirVector.y).angleDeg()

    val attackShiftX = dirVector.normal.x * attackRange
    val attackShiftY = dirVector.normal.y * attackRange

    val attackRectX = attackShiftX + creature.state.pos.x
    val attackRectY = attackShiftY + creature.state.pos.y

    State { implicit gameState =>
      (
        modifyAbility(
          _.modify(_.state.hitbox).setTo(
            Some(
              Hitbox(
                pos = Vec2(attackRectX, attackRectY),
                width = width,
                height = height,
                rotation = theta,
                scale = scale
              )
            )
          )
        ),
        List()
      )
    }
  }

  def update(delta: Float)(implicit gameState: GameState): GameStateTransition =
    (state.stage match {
      case AbilityStage.Inactive =>
        if (state.justPerformed) updateStateToChannel()
        else Monoid[GameStateTransition].empty
      case AbilityStage.Channel =>
        onChannelUpdate() |+|
          (
            if (state.stageTimer.time > getAbility.channelTime)
              updateStateToActive()
            else
              Monoid[GameStateTransition].empty
          )
      case AbilityStage.Active =>
        onActiveUpdate() |+| (
          if (state.stageTimer.time > getAbility.activeTime)
            updateStateToInactive()
          else
            Monoid[GameStateTransition].empty
        )

    }) |+| updateTimers(delta)

  private def updateStateToInactive()(implicit gameState: GameState): GameStateTransition =
    onInactiveStart() |+|
      State { implicit gameState =>
        (
          modifyAbility(
            _.modify(_.state.stage)
              .setTo(AbilityStage.Inactive)
              .modify(_.state.stageTimer)
              .using(_.restart())
          ),
          List(AbilityBodyDestroyEvent(id))
        )
      }

  private def updateStateToActive()(implicit gameState: GameState): GameStateTransition =
    onActiveStart() |+|
      State { implicit gameState =>
        (
          modifyAbility(
            _.modify(_.state.stage)
              .setTo(AbilityStage.Active)
              .modify(_.state.stageTimer)
              .using(_.restart())
          ),
          List(AbilityBodyActivateEvent(id))
        )
      }

  private def updateStateToChannel()(implicit gameState: GameState): GameStateTransition =
    onChannelStart() |+|
      State { implicit gameState =>
        (
          modifyAbility(
            _.modify(_.state.stage)
              .setTo(AbilityStage.Channel)
              .modify(_.state.stageTimer)
              .using(_.restart())
              .modify(_.state.stageTimer)
              .using(_.restart())
              .modify(_.state.justPerformed)
              .setTo(false)
          ),
          List()
        )
      }

  private def updateTimers(delta: Float): GameStateTransition =
    State { implicit gameState =>
      (gameState.pipe(implicit gameState => modifyAbility(_.modify(_.state.stageTimer).using(_.update(delta)))), List())
    }

  def forceStop(): GameStateTransition = {
    if (state.stage != AbilityStage.Inactive)
      State { implicit gameState =>
        (
          gameState.pipe(implicit gameState => modifyAbility(_.modify(_.state.stage).setTo(AbilityStage.Inactive))),
          List(AbilityBodyDestroyEvent(id))
        )
      }
    else Monoid[GameStateTransition].empty

  }

  def copy(state: AbilityState = state): Ability
}

object Ability {
  def abilityByName(creatureId: CreatureId, name: String): Ability =
    name match {
      case "slash" => SlashAbility(AbilityId.derive(creatureId, name), creatureId)
    }

}
