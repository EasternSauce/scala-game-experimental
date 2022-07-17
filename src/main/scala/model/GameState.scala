package model

import cats.data.State
import cats.implicits.toTraverseOps
import com.softwaremill.quicklens._
import game.ExternalEvent
import model.WorldDirection.WorldDirection

case class GameState(creatures: Map[String, Creature] = Map(), currentPlayer: String, currentAreaId: String) {}

object GameState {
  def creatureLens(creatureId: String)(implicit gameState: GameState): PathModify[GameState, CreatureState] =
    modify(gameState)(_.creatures.at(creatureId).state)

  def creature(creatureId: String)(implicit gameState: GameState): Creature =
    gameState.creatures(creatureId)

  def updateCreatures(delta: Float)(implicit gameState: GameState): State[GameState, List[ExternalEvent]] = {
    gameState.creatures.map { case (_, creature) => creature.update(delta) }.toList.sequence.map(_.flatten)
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

    val wasMoving = creature(gameState.currentPlayer).isMoving
    val isMoving = movingDir != Vec2(0, 0)

    List(
      {
        (wasMoving, isMoving) match {
          case (false, true) =>
            creature(gameState.currentPlayer).startMoving()
          case (true, false) =>
            creature(gameState.currentPlayer).stopMoving()
          case _ => State.pure[GameState, List[ExternalEvent]](List())

        }
      }, {
        if (isMoving) creature(gameState.currentPlayer).moveInDir(movingDir)
        else State.pure[GameState, List[ExternalEvent]](List())
      }
    ).sequence.map(_.flatten)

  }

}
