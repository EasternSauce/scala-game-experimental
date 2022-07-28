package com.easternsauce.model.ability
import com.easternsauce.model.{GameState, Vec2}
import com.easternsauce.model.ids.{AbilityId, CreatureId}

import scala.util.chaining.scalaUtilChainingOps

case class SlashAbility(state: AbilityState) extends Ability {
  val cooldownTime: Float = 1f

  val textureWidth: Int = 40
  val textureHeight: Int = 40
  val activeTime: Float = 0.3f
  val channelTime: Float = 0.3f
  val channelSpriteType: String = "slash_windup"
  val activeSpriteType: String = "slash"
  val channelFrameCount: Int = 6
  val activeFrameCount: Int = 6
  val channelFrameDuration: Float = 0.05f
  val activeFrameDuration: Float = 0.05f

  override def onActiveStart()(implicit gameState: GameState): GameState =
    gameState
      .pipe(implicit gameState => updateHitbox())

  override def onActiveUpdate()(implicit gameState: GameState): GameState =
    gameState
      .pipe(implicit gameState => updateHitbox())

  override def onChannelStart()(implicit gameState: GameState): GameState =
    gameState
      .pipe(implicit gameState => updateHitbox())

  override def onChannelUpdate()(implicit gameState: GameState): GameState =
    gameState
      .pipe(implicit gameState => updateHitbox())

  override def onInactiveStart()(implicit gameState: GameState): GameState = gameState

  override def copy(state: AbilityState = state): Ability = SlashAbility(state)

}

object SlashAbility {
  def apply(id: AbilityId, creatureId: CreatureId): Ability =
    new SlashAbility(AbilityState(id = id, creatureId = creatureId, attack = Some(Attack(Vec2(0,1), Hitbox(Vec2(0,0), 1, 1, 0, 1)))))
}
