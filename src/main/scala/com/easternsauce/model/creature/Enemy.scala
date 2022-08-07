package com.easternsauce.model.creature

import cats.Monoid
import cats.data.State
import cats.implicits.catsSyntaxSemigroup
import com.easternsauce.game.ExternalEvent
import com.easternsauce.model.GameState
import com.easternsauce.model.GameState.{GameStateTransition, getCreature, modifyCreature}
import com.softwaremill.quicklens.ModifyPimp

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

    if (potentialTarget.nonEmpty && this.isAlive && potentialTarget.get.isAlive) {

      val vectorTowardsTarget = getCreature.state.pos.vectorTowards(potentialTarget.get.state.pos)

      (if (getCreature.state.targetCreatureId.isEmpty || getCreature.state.targetCreatureId != potentialTargetId)
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
       else Monoid[GameStateTransition].empty) |+|
        (if (
           potentialTarget.get.state.pos.distance(getCreature.state.pos) > 3f && potentialTarget.get.state.pos
             .distance(getCreature.state.pos) < enemySearchDistance
         ) {
           if (getCreature.state.pathTowardsTarget.nonEmpty && getCreature.state.pathTowardsTarget.get.nonEmpty) {
             val path = getCreature.state.pathTowardsTarget.get
             val nextNodeOnPath = path.head
             if (getCreature.state.pos.distance(nextNodeOnPath) < 2f) {
               State[GameState, List[ExternalEvent]] { implicit gameState =>
                 (
                   modifyCreature(creature => creature.modify(_.state.pathTowardsTarget).setTo(Some(path.drop(1)))),
                   List()
                 )
               }
             } else getCreature.moveInDir(getCreature.state.pos.vectorTowards(nextNodeOnPath))
           } else {
             getCreature.moveInDir(vectorTowardsTarget)
           }
         } else Monoid[GameStateTransition].empty) |+|
        (if (potentialTarget.get.state.pos.distance(getCreature.state.pos) < 3f) getCreature.attack(vectorTowardsTarget)
         else Monoid[GameStateTransition].empty) |+|
        ???

    } else ??? // TODO: change
    //      gameState
    //        .modifyGameStateCreature(creatureId) {
    //
    //            //        .modify(_.state.targetCreatureId)
    //            //        .setTo(potentialTarget.map(_.state.id))
    //            .pipeIf(
    //
    //            ) { creature =>
    //
    //            }
    //        }
    //        .pipeIf(potentialTarget.get.pos.distance(creature.pos) < 3f) { gameState =>
    //          gameState.creatures(creatureId).attack(gameState, vectorTowardsTarget)
    //        }
    //        .pipe(gameState => {
    //          val creature = gameState.creatures(creatureId)
    //
    //          val pickedAbilityId = pickAbilityToUse(gameState)
    //          if (
    //            state.useAbilityTimer.time > useAbilityTimeout + creature.state.inbetweenAbilitiesTime && abilityUsages.nonEmpty && pickedAbilityId.nonEmpty
    //          ) {
    //            gameState
    //              .modifyGameStateCreature(creatureId) {
    //                _.modify(_.state.actionDirVector)
    //                  .setTo(creature.pos.vectorTowards(potentialTarget.get.pos))
    //                  .pipe(_.modify(_.state.useAbilityTimer).using(_.restart()))
    //                  .modify(_.state.inbetweenAbilitiesTime)
    //                  .setTo(Random.between(2f, 6f))
    //              }
    //              .performAbility(creatureId, pickedAbilityId.get)
    //
    //          } else gameState
    //        })
    //    } else
    //      gameState.modifyGameStateCreature(creatureId) { _.modify(_.state.targetCreatureId).setTo(None).stopMoving() }

  }

}
