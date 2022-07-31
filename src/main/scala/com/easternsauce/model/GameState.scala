package com.easternsauce.model

import cats._
import cats.data.State
import cats.implicits._
import com.badlogic.gdx.{Gdx, Input}
import com.easternsauce.game._
import com.easternsauce.game.physics.PhysicsEngineController
import com.easternsauce.model.WorldDirection.WorldDirection
import com.easternsauce.model.ability.{Ability, AbilityStage, Projectile}
import com.easternsauce.model.creature.Creature
import com.easternsauce.model.ids.{AbilityId, AreaId, CreatureId, ProjectileId}
import com.softwaremill.quicklens._

import scala.util.chaining.scalaUtilChainingOps

case class GameState(
  creatures: Map[CreatureId, Creature] = Map(),
  abilities: Map[AbilityId, Ability] = Map(),
  projectiles: Map[ProjectileId, Projectile] = Map(),
  currentPlayerId: CreatureId,
  currentAreaId: AreaId,
  currentAreaInitialized: Boolean
)

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

  def getCreature(implicit creatureId: CreatureId, gameState: GameState): Creature =
    gameState.creatures(creatureId)

  def modifyAbility(action: Ability => Ability)(implicit abilityId: AbilityId, gameState: GameState): GameState =
    modify(gameState)(_.abilities.at(abilityId)).using(action)

  def getAbility(implicit abilityId: AbilityId, gameState: GameState): Ability =
    gameState.abilities(abilityId)

  def modifyProjectile(
    action: Projectile => Projectile
  )(implicit projectileId: ProjectileId, gameState: GameState): GameState =
    modify(gameState)(_.projectiles.at(projectileId)).using(action)

  def getProjectile(implicit projectileId: ProjectileId, gameState: GameState): Projectile =
    gameState.projectiles(projectileId)

  def player(implicit gameState: GameState): Creature = gameState.creatures(gameState.currentPlayerId)

  def handlePlayerMovementInput(input: Map[WorldDirection, Boolean])(implicit gameState: GameState): GameState = {
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

    val movingDir = Vec2(movingDirX.toFloat, movingDirY.toFloat)

    implicit val playerId: CreatureId = gameState.currentPlayerId

    val wasMoving = getCreature.isMoving
    val isMoving = movingDir != Vec2(0, 0)

    val mouseClicked = Gdx.input.isButtonJustPressed(Input.Buttons.LEFT) // TODO: this is temp

    val mouseX = Gdx.input.getX
    val mouseY = Gdx.input.getY

    val centerX = Gdx.graphics.getWidth / 2f
    val centerY = Gdx.graphics.getHeight / 2f

    val mouseDirVector = Vec2(mouseX - centerX, (Gdx.graphics.getHeight - mouseY) - centerY).normal

    gameState
      .pipe(implicit gameState => runMovingLogic(wasMoving, isMoving, movingDir))
      .pipe(implicit gameState => {
        implicit val abilityId: AbilityId = AbilityId.derive(gameState.currentPlayerId, "slash")
        if (mouseClicked) {
          getAbility
            .perform()
            .pipe(implicit gameState => modifyAbility(_.modify(_.state.dirVector).setTo(Some(mouseDirVector))))
        } else gameState
      })

  }

  def handleCreaturePhysicsUpdate(creatureId: CreatureId): GameStateTransition = {

    val physicsPlayerPos = Vec2.fromVector2(PhysicsEngineController.creatureBodies(creatureId).pos)

    State { implicit gameState =>
      (modifyCreature(_.modify(_.state.pos).setTo(physicsPlayerPos))(creatureId, gameState), List())
    }
  }

  def runMovingLogic(wasMoving: Boolean, isMoving: Boolean, movingDir: Vec2)(implicit
    gameState: GameState
  ): GameState = {
    implicit val playerId: CreatureId = gameState.currentPlayerId
    gameState
      .pipe(
        implicit gameState =>
          (wasMoving, isMoving) match {
            case (false, true) =>
              getCreature.startMoving()
            case (true, false) =>
              getCreature.stopMoving()
            case _ => gameState

          }
      )
      .pipe(
        implicit gameState =>
          if (isMoving) getCreature.moveInDir(movingDir)
          else gameState
      )
  }

  def performAction(gameStateAction: GameStateAction): GameStateTransition = {
    gameStateAction match {
      case AbilityUpdateAction(abilityId, delta) =>
        implicit val _abilityId: AbilityId = abilityId
        State { implicit gameState =>
          val events =
            (if (
               getAbility.state.stage == AbilityStage.Channel && getAbility.state.stageTimer.time > getAbility.channelTime
             ) List(AbilityBodyActivateEvent(abilityId))
             else List()) ++
              (if (
                 getAbility.state.stage == AbilityStage.Active && getAbility.state.stageTimer.time > getAbility.activeTime
               ) List(AbilityBodyDestroyEvent(abilityId))
               else List())
          (getAbility.update(delta), events)
        }
      case CreatureInitAction(_) => Monoid[GameStateTransition].empty
//        implicit val _creatureId: CreatureId = creatureId
//        State { implicit gameState =>
//          (
//            getCreature.init(),
//            getCreature.abilityNames
//              .map(AbilityId.derive(creatureId, _))
//              .flatMap(
//                abilityId => List(AbilityBodyCreateEvent(abilityId), AbilitySpriteRendererCreateEvent(abilityId))
//              )
//          )
//        }
      case CreatureUpdateAction(creatureId, delta) =>
        implicit val _creatureId: CreatureId = creatureId
        State { implicit gameState => (getCreature.update(delta), List()) }
      case AreaInitializeAction(areaId) =>
        State { implicit gameState =>
          {
            val creatureInits = gameState.creatures.values.filter(_.state.areaId == areaId).foldLeft(gameState) {
              case (gameState, creature) => creature.init()(gameState)
            }
            val events: List[ExternalEvent] = gameState.creatures.values
              .filter(_.state.areaId == areaId)
              .flatMap(creature => creature.abilityNames.map(AbilityId.derive(creature.state.id, _)))
              .toList
              .flatMap(
                abilityId => List(AbilityBodyCreateEvent(abilityId), AbilitySpriteRendererCreateEvent(abilityId))
              )
            (creatureInits.modify(_.currentAreaInitialized).setTo(true), events)
          }
        }

    }
  }
}

sealed trait GameStateAction

case class AbilityUpdateAction(abilityId: AbilityId, delta: Float) extends GameStateAction
case class CreatureInitAction(creatureId: CreatureId) extends GameStateAction
case class CreatureUpdateAction(creatureId: CreatureId, delta: Float) extends GameStateAction
case class AreaInitializeAction(areaId: AreaId) extends GameStateAction
