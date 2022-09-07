package com.easternsauce.model.ability
import cats.data.State
import com.easternsauce.model.GameState
import com.easternsauce.model.GameState.GameStateTransition
import com.easternsauce.model.ids.{AbilityId, CreatureId}

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

  override val scale: Float = 1.4f

  override def onActiveStart()(implicit gameState: GameState): GameStateTransition =
    updateHitbox()

  override def onActiveUpdate()(implicit gameState: GameState): GameStateTransition =
    updateHitbox()

  override def onChannelStart()(implicit gameState: GameState): GameStateTransition =
    updateHitbox()

  override def onChannelUpdate()(implicit gameState: GameState): GameStateTransition =
    updateHitbox()

  override def onInactiveStart()(implicit gameState: GameState): GameStateTransition =
    State { implicit gameState =>
      (gameState, List())
    }

  override def copy(state: AbilityState = state): Ability = SlashAbility(state)

}

object SlashAbility {
  def apply(id: AbilityId, creatureId: CreatureId): Ability =
    new SlashAbility(AbilityState(id = id, creatureId = creatureId))
}
