package model

import cats.data.State
import cats.implicits.toTraverseOps
import com.softwaremill.quicklens._
import game.WorldDirection.WorldDirection
import game.{ExternalEvent, Vec2, WorldDirection}

case class GameState(creatures: Map[String, Creature] = Map(), currentPlayer: String) {}

object GameState {
  def creatureLens(creatureId: String)(implicit gameState: GameState): PathModify[GameState, CreatureState] =
    modify(gameState)(_.creatures.at(creatureId).state)

  def creature(creatureId: String)(implicit gameState: GameState): Creature =
    gameState.creatures(creatureId)

  def updateCreatures()(implicit gameState: GameState): State[GameState, List[ExternalEvent]] = {
    gameState.creatures.map { case (_, creature) => creature.update() }.toList.sequence.map(_.flatten)
  }

  def player(implicit gameState: GameState): Creature = gameState.creatures(gameState.currentPlayer)

  def handlePlayerMovementInput(
    input: Map[WorldDirection, Boolean]
  )(implicit gameState: GameState): State[GameState, List[ExternalEvent]] = {
    val movingDirX = (input(WorldDirection.Left), input(WorldDirection.Right)) match {
      case (true, false) => -1
      case (false, true) => 1
      case _             => 0
    }
    val movingDirY = (input(WorldDirection.Down), input(WorldDirection.Up)) match {
      case (true, false) => -1
      case (false, true) => 1
      case _             => 0
    }

    val movingDir = Vec2(movingDirX, movingDirY)

    State { gameState =>
      (gameState.modify(_.creatures.at(gameState.currentPlayer).state.movingDir).setTo(movingDir), List())
    }
  }

}
