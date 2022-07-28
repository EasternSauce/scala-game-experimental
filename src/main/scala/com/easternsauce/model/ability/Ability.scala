package com.easternsauce.model.ability

import com.easternsauce.model.GameState
import com.easternsauce.model.GameState.modifyAbility
import com.easternsauce.model.ids.{AbilityId, CreatureId}
import com.softwaremill.quicklens.ModifyPimp

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

  implicit val id: AbilityId = state.id

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
                  .pipe(implicit gameState => state.attack.map(_.onActiveStart()).getOrElse(gameState))
              else gameState
          )
          .pipe(implicit gameState => onChannelUpdate())
          .pipe(implicit gameState => state.attack.map(_.onChannelUpdate()).getOrElse(gameState))
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
          .pipe(implicit gameState => state.attack.map(_.onActiveUpdate()).getOrElse(gameState))
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
        .pipe(implicit gameState => state.attack.map(_.onChannelStart()).getOrElse(gameState))
    else gameState
  }

  def copy(state: AbilityState = state): Ability
}

object Ability {
  def abilityByName(name: String, creatureId: CreatureId): Ability =
    name match {
      case "slash" => SlashAbility(AbilityId.derive(creatureId, name), creatureId)
    }
}
