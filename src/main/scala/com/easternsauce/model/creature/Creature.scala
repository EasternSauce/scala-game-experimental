package com.easternsauce.model.creature

import com.easternsauce.model.GameState.modifyCreature
import com.easternsauce.model.WorldDirection.WorldDirection
import com.easternsauce.model.ability.Ability
import com.easternsauce.model.ids.CreatureId
import com.easternsauce.model.{GameState, Vec2, WorldDirection}
import com.softwaremill.quicklens.ModifyPimp

trait Creature {
  val state: CreatureState

  val textureName: String
  val textureWidth: Int
  val textureHeight: Int
  val width: Float
  val height: Float
  val frameDuration: Float
  val frameCount: Int
  val neutralStanceFrame: Int
  val dirMap: Map[WorldDirection, Int]

  val speed: Float = 15f

  val abilityNames: List[String] = List()

  implicit val id: CreatureId = state.id

  def isMoving: Boolean = state.currentSpeed > 0f

  def facingDirection: WorldDirection = {
    state.movingDir.angleDeg() match {
      case angle if angle >= 45 && angle < 135  => WorldDirection.Up
      case angle if angle >= 135 && angle < 225 => WorldDirection.Left
      case angle if angle >= 225 && angle < 315 => WorldDirection.Down
      case _                                    => WorldDirection.Right
    }
  }

  def moveInDir(dir: Vec2)(implicit gameState: GameState): GameState = {
    modifyCreature(_.modify(_.state.movingDir).setTo(dir))
  }

  def startMoving()(implicit gameState: GameState): GameState = {

    modifyCreature(_.modify(_.state.currentSpeed).setTo(this.speed).modify(_.state.animationTimer).using(_.restart()))
  }

  def stopMoving()(implicit gameState: GameState): GameState = {
    modifyCreature(_.modify(_.state.currentSpeed).setTo(0f))
  }

  def isAlive = true // TODO

  def update(delta: Float)(implicit gameState: GameState): GameState = {
    updateTimers(delta)
  }

  def updateTimers(delta: Float)(implicit gameState: GameState): GameState = {
    modifyCreature(
      _.modifyAll(_.state.animationTimer)
        .using(_.update(delta))
    )

  }

  def init()(implicit gameState: GameState): GameState = {

    // init abilities
    abilityNames.foldLeft(gameState) {
      case (gameState, abilityName) =>
        val ability = Ability.abilityByName(abilityName, id)
        gameState.modify(_.abilities).using(_.updated(ability.id, ability))
    }
  }

  def copy(state: CreatureState): Creature
}
