package com.easternsauce.model

import cats._
import cats.data.State
import cats.implicits._
import com.badlogic.gdx.{Gdx, Input}
import com.easternsauce.game.ExternalEvent
import com.easternsauce.game.physics.PhysicsEngineController
import com.easternsauce.model.WorldDirection.WorldDirection
import com.easternsauce.model.ability.{Ability, Attack}
import com.easternsauce.model.creature.Creature
import com.easternsauce.model.ids.{AbilityId, AreaId, AttackId, CreatureId}
import com.softwaremill.quicklens._

case class GameState(
  creatures: Map[CreatureId, Creature] = Map(),
  abilities: Map[AbilityId, Ability] = Map(),
  attacks: Map[AttackId, Attack] = Map(),
  currentPlayerId: CreatureId,
  currentAreaId: AreaId
) {}

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

  def modifyCreature(action: Creature => Creature)(implicit creatureId: CreatureId, gameState: GameState): GameState =
    modify(gameState)(_.creatures.at(creatureId)).using(action)

  def creature(creatureId: CreatureId)(implicit gameState: GameState): Creature =
    gameState.creatures(creatureId)

  def modifyAbility(action: Ability => Ability)(implicit abilityId: AbilityId, gameState: GameState): GameState =
    modify(gameState)(_.abilities.at(abilityId)).using(action)

  def ability(abilityId: AbilityId)(implicit gameState: GameState): Ability =
    gameState.abilities(abilityId)

//  def modifyAttack(action: Attack => Attack)(implicit attackId: AttackId, gameState: GameState): GameState =
//    modify(gameState)(_.attacks.at(attackId)).using(action)

  def modifyAttack(action: AttackId => GameStateTransition)(implicit attackId: AttackId): GameStateTransition = {
    action(attackId)
//    State { implicit gameState => (modify(gameState)(_.attacks.at(attackId)).using(action), List()) }
  }

  def attack(attackId: AttackId)(implicit gameState: GameState): Attack =
    gameState.attacks(attackId)

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

    val mouseClicked = Gdx.input.isButtonJustPressed(Input.Buttons.LEFT) // TODO: this is temp

    runMovingLogic(gameState, wasMoving, isMoving, movingDir) |+|
      State { implicit gameState =>
        (
          if (mouseClicked) ability(AbilityId.derive(gameState.currentPlayerId, "slash")).perform() else gameState,
          List()
        )
      }

  }

  def handleCreaturePhysicsUpdate(creatureId: CreatureId): GameStateTransition = {

    val physicsPlayerPos = Vec2.fromVector2(PhysicsEngineController.creatureBodies(creatureId).pos)

    State { implicit gameState =>
      (modifyCreature(_.modify(_.state.pos).setTo(physicsPlayerPos))(creatureId, gameState), List())
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

  def performAction(gameStateAction: GameStateAction): GameStateTransition = {
    gameStateAction match {
      //    case OnChannelStartAction(abilityId) => State {implicit gameState => (ability(abilityId).onChannelStart(), List())}
      //    case OnChannelUpdateAction(abilityId) =>State {implicit gameState => (ability(abilityId).onChannelUpdate(), List())}
      //    case OnActiveStartAction(abilityId) =>State {implicit gameState => (ability(abilityId).onActiveStart(), List())}
      //    case OnActiveUpdateAction(abilityId) =>State {implicit gameState => (ability(abilityId).onActiveUpdate(), List())}
      case AbilityUpdateAction(abilityId, delta) =>
        State { implicit gameState => (ability(abilityId).update(delta), List()) }
      case CreatureInitAction(creatureId) => State { implicit gameState => (creature(creatureId).init(), List()) }

    }
  }
}

sealed trait GameStateAction

//case class OnChannelStartAction(abilityId: AbilityId) extends GameStateAction
//case class OnChannelUpdateAction(abilityId: AbilityId) extends GameStateAction
//case class OnActiveStartAction(abilityId: AbilityId) extends GameStateAction
//case class OnActiveUpdateAction(abilityId: AbilityId) extends GameStateAction
case class AbilityUpdateAction(abilityId: AbilityId, delta: Float) extends GameStateAction
case class CreatureInitAction(creatureId: CreatureId) extends GameStateAction
