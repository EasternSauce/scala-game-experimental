package com.easternsauce.model.ability
import cats.kernel.Monoid
import com.easternsauce.model.GameState
import com.easternsauce.model.GameState.GameStateTransition

case class ArrowProjectile(override val state: ProjectileState) extends Projectile {

  override def onChannelStart()(implicit gameState: GameState): GameStateTransition = Monoid[GameStateTransition].empty

  override def onChannelUpdate()(implicit gameState: GameState): GameStateTransition = Monoid[GameStateTransition].empty

  override def onActiveStart()(implicit gameState: GameState): GameStateTransition = Monoid[GameStateTransition].empty

  override def onActiveUpdate()(implicit gameState: GameState): GameStateTransition = Monoid[GameStateTransition].empty

  override def copy(state: ProjectileState): Projectile = ArrowProjectile(state)
}
