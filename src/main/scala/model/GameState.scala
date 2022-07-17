package model

import cats.data.State
import cats.implicits.toTraverseOps
import com.softwaremill.quicklens._
import game.{ExternalEvent, Vec2, WorldDirection}
import game.WorldDirection.WorldDirection

case class GameState(creatures: Map[String, Creature] = Map(),
                     currentPlayer: String) {}

object GameState {
  def creatureLens(creatureId: String)(implicit gameState: GameState): PathModify[GameState, CreatureState] =
    modify(gameState)(_.creatures.at(creatureId).state)

  def creatureState(creatureId: String)(implicit gameState: GameState): CreatureState =
    gameState.creatures(creatureId).state

  def updateCreatures()(implicit gameState: GameState): State[GameState, List[ExternalEvent]] = {
    gameState.creatures.map { case (_, creature) => creature.update() }.toList.sequence.map(_ => List())
  }

  def handlePlayerMovementInput(input: Map[WorldDirection, Boolean])(implicit gameState: GameState): State[GameState, Unit] = {
    val movingDirX = (input(WorldDirection.Left), input(WorldDirection.Right)) match {
      case (true, false) => -1
      case (false, true) => 1
      case _ => 0
    }
    val movingDirY = (input(WorldDirection.Down), input(WorldDirection.Up)) match {
      case (true, false) => -1
      case (false, true) => 1
      case _ => 0
    }

    val movingDir = Vec2(movingDirX, movingDirY)

    State.modify{ gameState =>
      gameState.modify(_.creatures.at(gameState.currentPlayer).state.movingDir).setTo(movingDir)
    }
  }



}
