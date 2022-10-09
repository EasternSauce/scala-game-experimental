package com.easternsauce.model.ability
import cats.data.State
import com.easternsauce.model.GameState
import com.easternsauce.model.GameState.GameStateTransition
import com.easternsauce.model.ids.{AbilityId, AreaId, CreatureId}

case class SlashAbility(state: AbilityState) extends Ability {
  val cooldownTime: Float = 0.3f

  override val attackPhases: List[AttackPhase] =
    List(
      AttackPhase(
        animation = SlashAnimationData,
        knockbackVelocity = 5f,
        attackActiveSoundId = Some("swoosh"),
        attackActiveSoundPitch = Some(1.0f)
      )
    )

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
  def apply(name: String, creatureId: CreatureId, areaId: AreaId): Ability =
    new SlashAbility(
      AbilityState(id = AbilityId.derive(creatureId, areaId, name), creatureId = creatureId, areaId = areaId)
    )
}
