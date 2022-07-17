package model

import cats.data.State
import game.WorldDirection.WorldDirection
import game.{ExternalEvent, Vec2, WorldDirection}

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

  def isMoving: Boolean = state.movingDir != Vec2(0, 0)

  def facingDirection: WorldDirection = {
    state.movingDir.angleDeg() match {
      case angle if angle >= 45 && angle < 135  => WorldDirection.Up
      case angle if angle >= 135 && angle < 225 => WorldDirection.Left
      case angle if angle >= 225 && angle < 315 => WorldDirection.Down
      case _                                    => WorldDirection.Right
    }
  }

  def update(): State[GameState, List[ExternalEvent]]

  def copy(state: CreatureState): Creature
}
