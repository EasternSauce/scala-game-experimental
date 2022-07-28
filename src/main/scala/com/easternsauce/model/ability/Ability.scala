package com.easternsauce.model.ability

import com.badlogic.gdx.math.Vector2
import com.easternsauce.game.Constants
import com.easternsauce.model.GameState.{getCreature, modifyAbility}
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

  val attack: Option[Attack] = None

  val attackRange: Float = 1.8f

  implicit val id: AbilityId = state.id

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

  def runStageLogic()(implicit gameState: GameState): GameState =
    state.stage match {
      case AbilityStage.Inactive => gameState
      case AbilityStage.Channel =>
        gameState
          .pipe(
            implicit gameState =>
              if (state.stageTimer.time > channelTime)
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
                  .pipe(implicit gameState => onActiveStart())
//                  .pipe(implicit gameState => state.attack.map(_.onActiveStart()).getOrElse(gameState))
              else gameState
          )
          .pipe(implicit gameState => onChannelUpdate())
//          .pipe(implicit gameState => state.attack.map(_.onChannelUpdate()).getOrElse(gameState))
      case AbilityStage.Active =>
        gameState
          .pipe(
            implicit gameState =>
              if (state.stageTimer.time > activeTime)
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
                  .pipe(implicit gameState => onInactiveStart())
              else gameState
          )
          .pipe(implicit gameState => onActiveUpdate())
//          .pipe(implicit gameState => state.attack.map(_.onActiveUpdate()).getOrElse(gameState))
    }

  def update(delta: Float)(implicit gameState: GameState): GameState = {
    gameState
      .pipe(implicit gameState => runStageLogic())
      .pipe(implicit gameState => updateTimers(delta))
  }

  def perform()(implicit gameState: GameState): GameState = {
    if (ableToPerform)
      gameState
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
        .pipe(implicit gameState => onChannelStart())
//        .pipe(implicit gameState => state.attack.map(_.onChannelStart()).getOrElse(gameState))
    else gameState
  }

  def updateHitbox()(implicit gameState: GameState): GameState = {
    if (attack.nonEmpty) {
      val dirVector = state.attack.get.dirVector match {
        case dirVector if dirVector.length <= 0 => Vec2(1, 0).normal
        case dirVector                          => dirVector
      }

      val creature = getCreature(state.creatureId)

      val theta = new Vector2(dirVector.x, dirVector.y).angleDeg()

      val attackShiftX = dirVector.normal.x * attackRange
      val attackShiftY = dirVector.normal.y * attackRange

      val attackRectX = attackShiftX + creature.state.pos.x
      val attackRectY = attackShiftY + creature.state.pos.y

      modifyAbility(
        _.modify(_.state.attack.each.hitbox).setTo(
          Hitbox(pos = Vec2(attackRectX, attackRectY), width = width, height = height, rotation = theta, scale = scale)
        )
      )
    } else gameState

  }

  def copy(state: AbilityState = state): Ability
}

object Ability {
  def abilityByName(name: String, creatureId: CreatureId): Ability =
    name match {
      case "slash" => SlashAbility(AbilityId.derive(creatureId, name), creatureId)
    }
}
