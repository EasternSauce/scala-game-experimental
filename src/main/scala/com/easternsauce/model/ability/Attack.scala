package com.easternsauce.model.ability

import com.easternsauce.model.GameState.GameStateTransition

case class Attack() {
  def onChannelStart(): GameStateTransition = ???
}
