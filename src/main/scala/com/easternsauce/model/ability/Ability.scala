package com.easternsauce.model.ability

import cats.data.State
import com.badlogic.gdx.math.Vector2
import com.easternsauce.game.{AbilityBodyActivateEvent, AbilityBodyDestroyEvent, Constants}
import com.easternsauce.model.GameState.{GameStateTransition, getAbility, getCreature, modifyAbility}
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

  def ableToPerform: Boolean = state.stage == AbilityStage.Inactive && state.stageTimer.time > cooldownTime

  def updateTimers(delta: Float)(implicit gameState: GameState): GameState = {
    modifyAbility(_.modify(_.state.stageTimer).using(_.update(delta)))
  }

  def onActiveStart()(implicit gameState: GameState): GameState

  def onActiveUpdate()(implicit gameState: GameState): GameState

  def onChannelStart()(implicit gameState: GameState): GameState

  def onChannelUpdate()(implicit gameState: GameState): GameState

  def onInactiveStart()(implicit gameState: GameState): GameState

  def perform()(implicit gameState: GameState): GameState = {
    if (ableToPerform)
      gameState
        .pipe(implicit gameState => modifyAbility(_.modify(_.state.justPerformed).setTo(true)))
    else gameState
  }

  def updateHitbox()(implicit gameState: GameState): GameState = {
    if (state.dirVector.nonEmpty) {
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
      )
    } else throw new RuntimeException("trying to update hitbox without setting dir vector!")

  }

  def copy(state: AbilityState = state): Ability
}

object Ability {
  def abilityByName(name: String, creatureId: CreatureId): Ability =
    name match {
      case "slash" => SlashAbility(AbilityId.derive(creatureId, name), creatureId)
    }

  def updateAbility(abilityId: AbilityId, delta: Float): GameStateTransition = {
    implicit val _abilityId: AbilityId = abilityId
    State { implicit gameState =>
      val events =
        (if (
          getAbility.state.stage == AbilityStage.Channel && getAbility.state.stageTimer.time > getAbility.channelTime
        ) List(AbilityBodyActivateEvent(abilityId))
        else List()) ++
          (if (
            getAbility.state.stage == AbilityStage.Active && getAbility.state.stageTimer.time > getAbility.activeTime
          ) List(AbilityBodyDestroyEvent(abilityId))
          else List())
      (
        gameState
          .pipe(
            implicit gameState =>
              getAbility.state.stage match {
                case AbilityStage.Inactive =>
                  if (getAbility.state.justPerformed) {
                    gameState
                      .pipe(implicit gameState => getAbility.onChannelStart())
                      .pipe(implicit gameState => modifyAbility(_.modify(_.state.justPerformed).setTo(false)))
                      .pipe(
                        implicit gameState =>
                          modifyAbility(
                            _.modify(_.state.stage)
                              .setTo(AbilityStage.Channel)
                              .modify(_.state.stageTimer)
                              .using(_.restart())
                              .modify(_.state.stageTimer)
                              .using(_.restart())
                          )
                      )
                  } else gameState
                case AbilityStage.Channel =>
                  gameState
                    .pipe(
                      implicit gameState =>
                        if (getAbility.state.stageTimer.time > getAbility.channelTime)
                          gameState
                            .pipe(
                              implicit gameState =>
                                modifyAbility(
                                  _.modify(_.state.stage)
                                    .setTo(AbilityStage.Active)
                                    .modify(_.state.stageTimer)
                                    .using(_.restart())
                                )
                            )
                            .pipe(implicit gameState => getAbility.onActiveStart())
                        else gameState
                    )
                    .pipe(implicit gameState => getAbility.onChannelUpdate())
                case AbilityStage.Active =>
                  gameState
                    .pipe(
                      implicit gameState =>
                        if (getAbility.state.stageTimer.time > getAbility.activeTime)
                          gameState
                            .pipe(
                              implicit gameState =>
                                modifyAbility(
                                  _.modify(_.state.stage)
                                    .setTo(AbilityStage.Inactive)
                                    .modify(_.state.stageTimer)
                                    .using(_.restart())
                                )
                            )
                            .pipe(implicit gameState => getAbility.onInactiveStart())
                        else gameState
                    )
                    .pipe(implicit gameState => getAbility.onActiveUpdate())
              }
          )
          .pipe(implicit gameState => getAbility.updateTimers(delta)),
        events
      )
    }
  }
}
