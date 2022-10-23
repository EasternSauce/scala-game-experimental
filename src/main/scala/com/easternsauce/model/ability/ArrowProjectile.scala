package com.easternsauce.model.ability
import cats.kernel.Monoid
import com.easternsauce.model.GameState.GameStateTransition
import com.easternsauce.model.{GameState, Vec2}

case class ArrowProjectile(override val state: ProjectileState) extends Projectile {

  val width: Float = 2f
  val height: Float = 2f
  override def onChannelStart()(implicit gameState: GameState): GameStateTransition = Monoid[GameStateTransition].empty

  override def onChannelUpdate()(implicit gameState: GameState): GameStateTransition = Monoid[GameStateTransition].empty

  override def onActiveStart()(implicit gameState: GameState): GameStateTransition = Monoid[GameStateTransition].empty

  override def onActiveUpdate()(implicit gameState: GameState): GameStateTransition = Monoid[GameStateTransition].empty

  override def copy(state: ProjectileState): Projectile = ArrowProjectile(state)
}

object ArrowProjectile {
  def apply(ability: Ability, pos: Vec2, counter: Long): ArrowProjectile = {
    new ArrowProjectile(ProjectileState(ability, pos, counter))
  }
}
