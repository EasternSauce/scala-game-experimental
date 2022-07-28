package com.easternsauce.model.ability

import com.easternsauce.model.GameState.modifyProjectile
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

  def onChannelStart()(implicit gameState: GameState): GameState
  def onChannelUpdate()(implicit gameState: GameState): GameState
  def onActiveStart()(implicit gameState: GameState): GameState
  def onActiveUpdate()(implicit gameState: GameState): GameState

  def updateTimers(delta: Float)(implicit gameState: GameState): GameState = {
    modifyProjectile(_.modify(_.timer).using(_.update(delta)))
  }

  def runStageLogic()(implicit gameState: GameState): GameState =
    stage match {
      case ProjectileStage.Channel =>
        gameState
          .pipe(
            implicit gameState =>
              if (timer.time > channelTime)
                gameState
                  .pipe(
                    implicit gameState =>
                      modifyProjectile(
                        _.modify(_.stage)
                          .setTo(ProjectileStage.Active)
                          .modify(_.timer)
                          .using(_.restart())
                      )
                  )
                  .pipe(implicit gameState => onActiveStart())
              else gameState
          )
          .pipe(implicit gameState => onChannelUpdate())
      case ProjectileStage.Active =>
        gameState
          .pipe(
            implicit gameState =>
              if (timer.time > activeTime)
                gameState
                  .pipe(
                    implicit gameState =>
                      modifyProjectile(
                        _.modify(_.timer)
                          .using(_.restart())
                      )
                  )
              else gameState
          )
          .pipe(implicit gameState => onActiveUpdate())
    }

  def update(delta: Float)(implicit gameState: GameState): GameState = {
    gameState
      .pipe(implicit gameState => runStageLogic())
      .pipe(implicit gameState => updateTimers(delta))
  }

  def copy(id: ProjectileId = id, timer: SimpleTimer = timer, stage: ProjectileStage = stage): Projectile
}
