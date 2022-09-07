package com.easternsauce.model.ability

import cats.data.State
import cats.implicits.catsSyntaxSemigroup
import cats.kernel.Monoid
import com.easternsauce.game.ExternalEvent
import com.easternsauce.model.GameState.{GameStateTransition, modifyProjectile}
import com.easternsauce.model.ability.ProjectileStage.ProjectileStage
import com.easternsauce.model.ids.ProjectileId
import com.easternsauce.model.{GameState, SimpleTimer}
import com.softwaremill.quicklens.ModifyPimp

import scala.util.chaining.scalaUtilChainingOps

trait Projectile {
  val id: ProjectileId
  val timer: SimpleTimer
  val stage: ProjectileStage
  val channelTime: Float
  val activeTime: Float

  implicit val _id: ProjectileId = id

  def onChannelStart()(implicit gameState: GameState): GameStateTransition
  def onChannelUpdate()(implicit gameState: GameState): GameStateTransition
  def onActiveStart()(implicit gameState: GameState): GameStateTransition
  def onActiveUpdate()(implicit gameState: GameState): GameStateTransition

  private def updateTimers(delta: Float): GameStateTransition =
    State { implicit gameState =>
      (modifyProjectile(_.modify(_.timer).using(_.update(delta))), List())
    }

  def runStageLogic()(implicit gameState: GameState): GameStateTransition =
    stage match {
      case ProjectileStage.Channel =>
        onChannelUpdate() |+|
          (if (timer.time > channelTime)
             State[GameState, List[ExternalEvent]] { implicit gameState =>
               (
                 gameState
                   .pipe(
                     implicit gameState =>
                       modifyProjectile(
                         _.modify(_.stage)
                           .setTo(ProjectileStage.Active)
                           .modify(_.timer)
                           .using(_.restart())
                       )
                   ),
                 List()
               )
             } |+| onActiveStart()
           else Monoid[GameStateTransition].empty)
      case ProjectileStage.Active =>
        onActiveUpdate() |+|
          (if (timer.time > activeTime) State[GameState, List[ExternalEvent]] { implicit gameState =>
             (
               modifyProjectile(
                 _.modify(_.timer)
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

  def copy(id: ProjectileId = id, timer: SimpleTimer = timer, stage: ProjectileStage = stage): Projectile
}
