package com.easternsauce.model.creature

import cats.Monoid
import cats.data.State
import cats.implicits.catsSyntaxSemigroup
import com.easternsauce.game.ExternalEvent
import com.easternsauce.model.GameState.{GameStateTransition, getAbility, getCreature, modifyCreature}
import com.easternsauce.model.ids.AbilityId
import com.easternsauce.model.{GameState, Vec2, ids}
import com.softwaremill.quicklens.ModifyPimp

import scala.util.Random

trait Enemy extends Creature {
  override def isEnemy: Boolean = true
  override def isControlledAutomatically: Boolean = true
  val enemySearchDistance = 30f

  def findTarget()(implicit gameState: GameState): Option[Creature] = {
    val potentialTargets: List[Creature] = gameState.creatures.values.toList.filter(
      target =>
        target.state.areaId == getCreature.state.areaId && target.isPlayer && target.state.pos
          .distance(getCreature.state.pos) < enemySearchDistance
    )

    potentialTargets match {
      case List() => None
      case targets =>
        Some(targets.minBy(_.state.pos.distance(getCreature.state.pos)))
    }

  }

  override def updateAutomaticControls()(implicit gameState: GameState): GameStateTransition = {

    val potentialTarget = findTarget()
    val potentialTargetId = potentialTarget.map(_.state.id)

    val vectorTowardsTarget = getCreature.state.pos.vectorTowards(potentialTarget.get.state.pos)

    val handleTargetLost = handleTargetLost()

    if (potentialTarget.nonEmpty && this.isAlive && potentialTarget.get.isAlive)
      handleNewTarget(potentialTargetId) |+|
        handleFollowPath(potentialTarget, vectorTowardsTarget) |+|
        handleAttackTarget(potentialTarget, vectorTowardsTarget) |+|
        handleAbilityUsage(potentialTarget)
    else
      handleTargetLost()

  }

  private def handleTargetLost()(implicit gameState: GameState) = {
    State[GameState, List[ExternalEvent]] { implicit gameState =>
      (modifyCreature(_.modify(_.state.targetCreatureId).setTo(None)), List())
    } |+| getCreature.stopMoving()
  }

  private def handleAbilityUsage(
    potentialTarget: Option[Creature]
  )(implicit gameState: GameState): GameStateTransition = {

    val pickedAbilityName = pickAbilityToUse()
    if (
      getCreature.state.useAbilityTimer.time > useAbilityTimeout + getCreature.state.inbetweenAbilitiesTime && abilityUsages.nonEmpty && pickedAbilityName.nonEmpty
    ) {
      State[GameState, List[ExternalEvent]] { implicit gameState =>
        (
          modifyCreature(
            _.modify(_.state.actionDirVector)
              .setTo(getCreature.state.pos.vectorTowards(potentialTarget.get.state.pos))
              .modify(_.state.useAbilityTimer)
              .using(_.restart())
              .modify(_.state.inbetweenAbilitiesTime)
              .setTo(Random.between(2f, 6f))
          ),
          List()
        )
      } |+| getAbility(AbilityId.derive(getCreature.id, pickedAbilityName.get), gameState).perform()
    } else Monoid[GameStateTransition].empty

  }

  private def handleAttackTarget(potentialTarget: Option[Creature], vectorTowardsTarget: Vec2)(implicit
    gameState: GameState
  ): GameStateTransition = {
    if (potentialTarget.get.state.pos.distance(getCreature.state.pos) < 3f) getCreature.attack(vectorTowardsTarget)
    else Monoid[GameStateTransition].empty
  }

  private def handleFollowPath(potentialTarget: Option[Creature], vectorTowardsTarget: Vec2)(implicit
    gameState: GameState
  ): GameStateTransition = {
    if (
      potentialTarget.get.state.pos.distance(getCreature.state.pos) > 3f && potentialTarget.get.state.pos
        .distance(getCreature.state.pos) < enemySearchDistance
    ) {
      if (getCreature.state.pathTowardsTarget.nonEmpty && getCreature.state.pathTowardsTarget.get.nonEmpty) {
        val path = getCreature.state.pathTowardsTarget.get
        val nextNodeOnPath = path.head
        if (getCreature.state.pos.distance(nextNodeOnPath) < 2f) {
          State[GameState, List[ExternalEvent]] { implicit gameState =>
            (modifyCreature(creature => creature.modify(_.state.pathTowardsTarget).setTo(Some(path.drop(1)))), List())
          }
        } else getCreature.moveInDir(getCreature.state.pos.vectorTowards(nextNodeOnPath))
      } else {
        getCreature.moveInDir(vectorTowardsTarget)
      }
    } else Monoid[GameStateTransition].empty
  }

  private def handleNewTarget(
    potentialTargetId: Option[ids.CreatureId]
  )(implicit gameState: GameState): GameStateTransition = {
    if (getCreature.state.targetCreatureId.isEmpty || getCreature.state.targetCreatureId != potentialTargetId)
      State[GameState, List[ExternalEvent]] { implicit gameState =>
        (
          modifyCreature(
            _.modify(_.state.forcePathCalculation)
              .setTo(true)
              .modify(_.state.targetCreatureId)
              .setTo(potentialTargetId)
              .modify(_.state.pathTowardsTarget)
              .setTo(None)
          ),
          List()
        )
      }
    else Monoid[GameStateTransition].empty
  }

  def pickAbilityToUse()(implicit gameState: GameState): Option[String] = {

    if (getCreature.state.targetCreatureId.nonEmpty) {
      val targetCreature = gameState.creatures(getCreature.state.targetCreatureId.get)

      val filteredAbilityUsages = abilityUsages.filter {
        case (abilityName, usage) =>
          getCreature.state.life / getCreature.state.maxLife <= usage.lifeThreshold && getCreature.state.pos
            .distance(targetCreature.state.pos) > usage.minimumDistance &&
            getCreature.state.pos.distance(targetCreature.state.pos) < usage.maximumDistance && !gameState
            .abilities(AbilityId.derive(getCreature.state.id, abilityName))
            .onCooldown
      }

      var completeWeight = 0.0f
      for (abilityUsage <- filteredAbilityUsages.values) {
        completeWeight += abilityUsage.weight
      }
      val r = Math.random * completeWeight
      var countWeight = 0.0
      for (abilityUsage <- filteredAbilityUsages) {
        val (key, value) = abilityUsage
        countWeight += value.weight
        if (countWeight > r) return Some(key)

      }
      None
    } else None

  }

  override def copy(params: CreatureState): Enemy = {
    // unreachable, always overriden; needed for quicklens to work in abstract class
    this
  }
}
