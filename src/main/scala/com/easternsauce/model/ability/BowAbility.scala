package com.easternsauce.model.ability
import cats.data.State
import cats.kernel.Monoid
import com.easternsauce.game.ExternalEvent
import com.easternsauce.model.GameState
import com.easternsauce.model.GameState.GameStateTransition
import com.easternsauce.model.ids.{AreaId, CreatureId, ProjectileId}
import com.softwaremill.quicklens.ModifyPimp

case class BowAbility(state: AbilityState) extends Ability {
  override val cooldownTime: Float = 1f
  override val attackPhases: List[AttackPhase] = List()

  override def onActiveStart()(implicit gameState: GameState): GameStateTransition =
    State[GameState, List[ExternalEvent]] { gameState =>
      (
        {
          val projectileCounter = state.projectileCounter

          val projectileId = ProjectileId.derive(this, projectileCounter)

          gameState
            .modify(_.projectiles)
            .using(_.updated(projectileId, ArrowProjectile(ProjectileState(this, projectileCounter))))

        },
        List()
      )
    }

  override def onActiveUpdate()(implicit gameState: GameState): GameStateTransition = Monoid[GameStateTransition].empty

  override def onChannelStart()(implicit gameState: GameState): GameStateTransition = Monoid[GameStateTransition].empty

  override def onChannelUpdate()(implicit gameState: GameState): GameStateTransition = Monoid[GameStateTransition].empty

  override def onInactiveStart()(implicit gameState: GameState): GameStateTransition = Monoid[GameStateTransition].empty

  override def copy(state: AbilityState): Ability = BowAbility(state)
}

object BowAbility {
  def apply(name: String, creatureId: CreatureId, areaId: AreaId): Ability =
    new BowAbility(AbilityState(name = name, creatureId = creatureId, areaId = areaId))
}
