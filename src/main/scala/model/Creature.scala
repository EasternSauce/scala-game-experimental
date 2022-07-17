package model

import cats.data.State
import game.{ExternalEvent, Vec2}
import game.WorldDirection.WorldDirection

trait Creature {
  val state: CreatureState
  
  def update(): State[GameState, List[ExternalEvent]]

  def copy(state: CreatureState): Creature
}
