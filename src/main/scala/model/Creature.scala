package model

import cats.data.State

trait Creature {
  val id: String
  val posX: Float
  val posY: Float
  
  def update(): State[GameState, Unit]

  def copy(id: String = id, posX: Float = posX, posY: Float = posY): Creature
}
