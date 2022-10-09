package com.easternsauce.model.area

import cats.Monoid
import com.easternsauce.model.GameState.GameStateTransition

case class Area(state: AreaState) {

  def reset(): GameStateTransition = Monoid[GameStateTransition].empty // TODO: area reset

}
