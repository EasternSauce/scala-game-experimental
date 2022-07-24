package model

import cats.data.State
import cats.implicits.toTraverseOps
import com.softwaremill.quicklens._
import game.ExternalEvent
import model.GameState.{creature, creatureLens}
import model.WorldDirection.WorldDirection

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

  def isMoving: Boolean = state.currentSpeed > 0f

  def facingDirection: WorldDirection = {
    state.movingDir.angleDeg() match {
      case angle if angle >= 45 && angle < 135  => WorldDirection.Up
      case angle if angle >= 135 && angle < 225 => WorldDirection.Left
      case angle if angle >= 225 && angle < 315 => WorldDirection.Down
      case _                                    => WorldDirection.Right
    }
  }

  def moveInDir(dir: Vec2): State[GameState, List[ExternalEvent]] = {
    State { implicit gameState: GameState =>
      (
        creatureLens(state.id)
          .using(_.modify(_.movingDir).setTo(dir)),
        List()
      )
    }
  }

  def startMoving(): State[GameState, List[ExternalEvent]] = {
    State { implicit gameState =>
      (
        creatureLens(state.id)
          .using(_.modify(_.currentSpeed).setTo(this.speed).modify(_.animationTimer).using(_.restart())),
        List()
      )
    }
  }

  def stopMoving(): State[GameState, List[ExternalEvent]] = {
    State { implicit gameState =>
      (
        creatureLens(state.id)
          .using(_.modify(_.currentSpeed).setTo(0f)),
        List()
      )
    }
  }

  def isAlive = true // TODO

  def update(delta: Float): State[GameState, List[ExternalEvent]] = {
    List(updateTimers(delta) /*, updatePosition()*/ ).sequence.map(_.flatten)
  }

  def updatePosition(): State[GameState, List[ExternalEvent]] = {
    State { implicit gameState =>
      (
        creatureLens(state.id).using(
          _.modify(_.pos.x)
            .using(_ + state.currentSpeed * creature(state.id).state.movingDir.x)
            .modify(_.pos.y)
            .using(_ + state.currentSpeed * creature(state.id).state.movingDir.y)
        ),
        List()
      )
    }
  }

  def updateTimers(delta: Float): State[GameState, List[ExternalEvent]] = {
    State { implicit gameState =>
      (
        creatureLens(state.id)
          .using(
            _.modifyAll(_.animationTimer)
              .using(_.update(delta))
          ),
        List()
      )

    }

  }

  def copy(state: CreatureState): Creature
}
