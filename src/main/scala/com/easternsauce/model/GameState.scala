package com.easternsauce.model

import cats._
import cats.data.State
import cats.implicits._
import com.badlogic.gdx.{Gdx, Input}
import com.easternsauce.game._
import com.easternsauce.game.physics.PhysicsEngineController
import com.easternsauce.model.WorldDirection.WorldDirection
import com.easternsauce.model.ability.{Ability, Projectile}
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

    implicit val abilityId: AbilityId = AbilityId.derive(gameState.currentPlayerId, "slash")

    runMovingLogic(wasMoving, isMoving, movingDir) |+|
      (if (mouseClicked) {
         getAbility.perform(mouseDirVector) |+|
           State { implicit gameState =>
             (
               gameState.pipe(
                 implicit gameState =>
                   modifyAbility(
                     _.modify(_.state.dirVector)
                       .setTo(Some(mouseDirVector))
                   )
               ),
               List()
             )
           }
       } else Monoid[GameStateTransition].empty)
  }

  def handleCreaturePhysicsUpdate(creatureId: CreatureId): GameStateTransition = {

    val physicsPlayerPos = Vec2.fromVector2(PhysicsEngineController.creatureBodies(creatureId).pos)

    State { implicit gameState =>
      (modifyCreature(_.modify(_.state.pos).setTo(physicsPlayerPos))(creatureId, gameState), List())
    }
  }

  def runMovingLogic(wasMoving: Boolean, isMoving: Boolean, movingDir: Vec2)(implicit
    gameState: GameState
  ): GameStateTransition = {
    implicit val playerId: CreatureId = gameState.currentPlayerId

    ((wasMoving, isMoving) match {
      case (false, true) =>
        getCreature.startMoving()
      case (true, false) =>
        getCreature.stopMoving()
      case _ =>
        Monoid[GameStateTransition].empty
    }) |+| (if (isMoving) getCreature.moveInDir(movingDir)
            else Monoid[GameStateTransition].empty)
  }

  def initializeArea(areaId: AreaId)(implicit gameState: GameState): GameStateTransition = {
    gameState.creatures.keys.toList.foldMap(implicit id => getCreature.init()) |+|
      State { implicit gameState =>
        {
          (
            gameState.modify(_.currentAreaInitialized).setTo(true),
            gameState.creatures.values
              .filter(_.state.areaId == areaId)
              .flatMap(creature => creature.abilityNames.map(AbilityId.derive(creature.state.id, _)))
              .toList
              .flatMap(
                abilityId => List(AbilityBodyCreateEvent(abilityId), AbilitySpriteRendererCreateEvent(abilityId))
              )
          )
        }
      }
  }

}
