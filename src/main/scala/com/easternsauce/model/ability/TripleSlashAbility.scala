package com.easternsauce.model.ability
import cats.data.State
import com.easternsauce.model.GameState
import com.easternsauce.model.GameState.GameStateTransition
import com.easternsauce.model.ids.{AbilityId, CreatureId}

case class TripleSlashAbility(state: AbilityState) extends Ability {
  val cooldownTime: Float = 0.3f

  override val attackPhases: List[AttackPhase] =
    List(
      AttackPhase(animation = SlashAnimation),
      AttackPhase(animation = BackslashAnimation),
      AttackPhase(animation = SlamAnimation)
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

  override def copy(state: AbilityState = state): Ability = TripleSlashAbility(state)

}

object TripleSlashAbility {
  def apply(id: AbilityId, creatureId: CreatureId): Ability =
    new TripleSlashAbility(AbilityState(id = id, creatureId = creatureId))
}