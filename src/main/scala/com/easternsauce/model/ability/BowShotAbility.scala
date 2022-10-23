package com.easternsauce.model.ability
import cats.data.State
import cats.kernel.Monoid
import com.easternsauce.game.{ExternalEvent, ProjectileSpawnEvent}
import com.easternsauce.model.GameState
import com.easternsauce.model.GameState.{GameStateTransition, getAbility, getCreature}
import com.easternsauce.model.ids.{AreaId, CreatureId, ProjectileId}
import com.softwaremill.quicklens.ModifyPimp

case class BowShotAbility(state: AbilityState) extends Ability {
  override val cooldownTime: Float = 1f
  override val attackPhases: List[AttackPhase] = List()

  override val defaultChannelTime: Float = 1f
  override val defaultActiveTime: Float = 1f

  override def onActiveStart()(implicit gameState: GameState): GameStateTransition = {

    val projectileCounter = state.projectileCounter

    val projectileId = ProjectileId.derive(this, projectileCounter)

    State[GameState, List[ExternalEvent]] { implicit gameState =>
      (
        {
          implicit val creatureId: CreatureId = getAbility.creatureId

          gameState
            .modify(_.projectiles)
            .using(
              _.updated(
                projectileId,
                ArrowProjectile(ability = this, pos = getCreature.state.pos, counter = projectileCounter)
              )
            )

        },
        List(ProjectileSpawnEvent(projectileId))
      )
    }
  }

  override def onActiveUpdate()(implicit gameState: GameState): GameStateTransition = Monoid[GameStateTransition].empty

  override def onChannelStart()(implicit gameState: GameState): GameStateTransition = Monoid[GameStateTransition].empty

  override def onChannelUpdate()(implicit gameState: GameState): GameStateTransition = Monoid[GameStateTransition].empty

  override def onInactiveStart()(implicit gameState: GameState): GameStateTransition = Monoid[GameStateTransition].empty

  override def copy(state: AbilityState): Ability = BowShotAbility(state)
}

object BowShotAbility {
  def apply(name: String, creatureId: CreatureId, areaId: AreaId): Ability =
    new BowShotAbility(AbilityState(name = name, creatureId = creatureId, areaId = areaId))
}
