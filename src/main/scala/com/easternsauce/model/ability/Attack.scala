package com.easternsauce.model.ability

import com.easternsauce.model.GameState

case class Attack() {
  def onChannelStart()(implicit gameState: GameState): GameState = ???
  def onChannelUpdate()(implicit gameState: GameState): GameState = ???
  def onActiveStart()(implicit gameState: GameState): GameState = ???
  def onActiveUpdate()(implicit gameState: GameState): GameState = ???
  def onInactiveStart()(implicit gameState: GameState): GameState = ???
}
