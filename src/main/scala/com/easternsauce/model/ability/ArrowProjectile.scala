package com.easternsauce.model.ability
import com.easternsauce.model.GameState
import com.easternsauce.model.GameState.GameStateTransition

case class ArrowProjectile(override val state: ProjectileState) extends Projectile {

  override def onChannelStart()(implicit gameState: GameState): GameStateTransition = ???

  override def onChannelUpdate()(implicit gameState: GameState): GameStateTransition = ???

  override def onActiveStart()(implicit gameState: GameState): GameStateTransition = ???

  override def onActiveUpdate()(implicit gameState: GameState): GameStateTransition = ???

  override def copy(state: ProjectileState): Projectile = ArrowProjectile(state)
}
