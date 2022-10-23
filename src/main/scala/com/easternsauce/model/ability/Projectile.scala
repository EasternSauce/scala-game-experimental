package com.easternsauce.model.ability

import cats.data.State
import cats.implicits.catsSyntaxSemigroup
import cats.kernel.Monoid
import com.easternsauce.game.ExternalEvent
import com.easternsauce.model.GameState
import com.easternsauce.model.GameState.{GameStateTransition, modifyProjectile}
import com.easternsauce.model.ids.{CreatureId, ProjectileId}
import com.softwaremill.quicklens.ModifyPimp

import scala.util.chaining.scalaUtilChainingOps

trait Projectile {
  val state: ProjectileState

  implicit val _id: ProjectileId = state.id
  implicit def creatureId: CreatureId = state.creatureId

  val width: Float
  val height: Float

  def onChannelStart()(implicit gameState: GameState): GameStateTransition
  def onChannelUpdate()(implicit gameState: GameState): GameStateTransition
  def onActiveStart()(implicit gameState: GameState): GameStateTransition
  def onActiveUpdate()(implicit gameState: GameState): GameStateTransition

  private def updateTimers(delta: Float): GameStateTransition =
    State { implicit gameState =>
      (modifyProjectile(_.modify(_.state.timer).using(_.update(delta))), List())
    }

  def runStageLogic()(implicit gameState: GameState): GameStateTransition =
    state.stage match {
      case ProjectileStage.Channel =>
        onChannelUpdate() |+|
          (if (state.timer.time > state.channelTime)
             State[GameState, List[ExternalEvent]] { implicit gameState =>
               (
                 gameState
                   .pipe(
                     implicit gameState =>
                       modifyProjectile(
                         _.modify(_.state.stage)
                           .setTo(ProjectileStage.Active)
                           .modify(_.state.timer)
                           .using(_.restart())
                       )
                   ),
                 List()
               )
             } |+| onActiveStart()
           else Monoid[GameStateTransition].empty)
      case ProjectileStage.Active =>
        onActiveUpdate() |+|
          (if (state.timer.time > state.activeTime) State[GameState, List[ExternalEvent]] { implicit gameState =>
             (
               modifyProjectile(
                 _.modify(_.state.timer)
                   .using(_.restart())
               ),
               List()
             )
           }
           else Monoid[GameStateTransition].empty)
    }

  def update(delta: Float)(implicit gameState: GameState): GameStateTransition =
    runStageLogic() |+|
      updateTimers(delta)

  def copy(state: ProjectileState = state): Projectile
}
