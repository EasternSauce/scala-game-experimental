package com.easternsauce.model.area

import cats.data.State
import com.easternsauce.game.ExternalEvent
import com.easternsauce.model.GameState
import com.easternsauce.model.GameState.{GameStateTransition, getCreature, modifyCreature}
import com.easternsauce.model.ids.{AreaId, CreatureId}
import com.softwaremill.quicklens._

case class Area(state: AreaState) {



  def reset(): GameStateTransition = ???

}
