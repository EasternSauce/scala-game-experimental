package model

import cats._
import cats.data.State
import cats.implicits._
import com.softwaremill.quicklens._
import game.ExternalEvent
import game.physics.PhysicsEngineController
import model.WorldDirection.WorldDirection

case class GameState(creatures: Map[String, Creature] = Map(), currentPlayerId: String, currentAreaId: String) {}

object GameState {
  type GameStateTransition = State[GameState, List[ExternalEvent]]

  implicit def gameStateMonoid: Monoid[GameStateTransition] =
    new Monoid[GameStateTransition] {
      def empty: GameStateTransition =
        State { state =>
          (state, List())
        }
      def combine(x: GameStateTransition, y: GameStateTransition): GameStateTransition = {
        List(x, y).sequence.map(_.flatten)
      }

    }

  def modifyCreature(creatureId: String)(action: Creature => Creature)(implicit gameState: GameState): GameState =
    modify(gameState)(_.creatures.at(creatureId)).using(action)

  def creature(creatureId: String)(implicit gameState: GameState): Creature =
    gameState.creatures(creatureId)

  def updateCreatures(delta: Float)(implicit gameState: GameState): GameStateTransition = {

    gameState.creatures.values.toList.foldMap { creature =>
      creature.update(delta)
    }
  }

  def player(implicit gameState: GameState): Creature = gameState.creatures(gameState.currentPlayerId)

  def handlePlayerMovementInput(
    input: Map[WorldDirection, Boolean]
  )(implicit gameState: GameState): GameStateTransition = {
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

    val wasMoving = creature(gameState.currentPlayerId).isMoving
    val isMoving = movingDir != Vec2(0, 0)

    val physicsPlayerPos = Vec2.fromVector2(PhysicsEngineController.creatureBodies(gameState.currentPlayerId).pos)

    runMovingLogic(gameState, wasMoving, isMoving, movingDir) |+|
      State { implicit gameState =>
        (modifyCreature(gameState.currentPlayerId)(_.modify(_.state.pos).setTo(physicsPlayerPos)), List())
      }

  }

  private def runMovingLogic(implicit
    gameState: GameState,
    wasMoving: Boolean,
    isMoving: Boolean,
    movingDir: Vec2
  ): GameStateTransition = {
    ((wasMoving, isMoving) match {
      case (false, true) =>
        creature(gameState.currentPlayerId).startMoving()
      case (true, false) =>
        creature(gameState.currentPlayerId).stopMoving()
      case _ => Monoid[GameStateTransition].empty

    }) |+|
      (if (isMoving) creature(gameState.currentPlayerId).moveInDir(movingDir)
       else Monoid[GameStateTransition].empty)

  }
}
