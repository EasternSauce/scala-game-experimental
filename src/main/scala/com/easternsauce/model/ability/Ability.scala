package com.easternsauce.model.ability

import cats.data.State
import cats.implicits.catsSyntaxSemigroup
import cats.kernel.Monoid
import com.badlogic.gdx.math.Vector2
import com.easternsauce.game.{AbilityBodyActivateEvent, AbilityBodyDeactivateEvent, ExternalEvent, PlaySoundEvent}
import com.easternsauce.model.GameState.{GameStateTransition, getAbility, getCreature, modifyAbility, modifyCreature}
import com.easternsauce.model.ids.{AbilityId, AreaId, CreatureId}
import com.easternsauce.model.{GameState, Vec2}
import com.softwaremill.quicklens._

import scala.util.chaining.scalaUtilChainingOps

trait Ability {
  val state: AbilityState
  val cooldownTime: Float

  val attackPhases: List[AttackPhase]

  def currentAttackPhase: AttackPhase = attackPhases(state.currentAttackPhase)
  def currentAnimation: AbilityAnimationData = currentAttackPhase.animation
  def currentAttackActiveSoundId: Option[String] = currentAttackPhase.attackActiveSoundId
  def currentAttackActiveSoundPitch: Option[Float] = currentAttackPhase.attackActiveSoundPitch

  val initSpeed: Float = 0f
  val activeAnimationLooping: Boolean = false
  val channelAnimationLooping: Boolean = false

  val attackRange: Float = 1.8f

  val currentAttackPhaseResetTimeout: Float = 1.3f

  implicit def id: AbilityId = state.id
  implicit def creatureId: CreatureId = state.creatureId

  def ableToPerform(implicit gameState: GameState): Boolean =
    getCreature.isAlive && !state.justPerformed && state.stage == AbilityStage.InactiveStage && state.stageTimer.time > cooldownTime && getCreature.state.stamina > 0 && !onCooldown

  def onCooldown: Boolean = false // TODO

  def knockbackVelocity: Float = currentAttackPhase.knockbackVelocity

  def perform(dir: Vec2)(implicit gameState: GameState): GameStateTransition =
    if (ableToPerform)
      State[GameState, List[ExternalEvent]] { implicit gameState =>
        (
          gameState.pipe(
            implicit gameState =>
              modifyAbility(
                _.modify(_.state.dirVector)
                  .setTo(Some(dir))
              )
          ),
          List()
        )
      } |+|
        State[GameState, List[ExternalEvent]] { implicit gameState =>
          (modifyCreature(_.modify(_.state.actionDirVector).setTo(dir)), List())
        } |+|
        State { implicit gameState =>
          (
            modifyAbility(
              _.modify(_.state.justPerformed)
                .setTo(true)
                .modify(_.state.dirVector)
                .setTo(Some(dir))
            ),
            List()
          )
        } |+|
        State { implicit gameState =>
          (
            modifyCreature(
              _.modify(_.state.staminaRegenerationDisabledTimer)
                .using(_.restart())
                .modify(_.state.isStaminaRegenerationDisabled)
                .setTo(true)
            ),
            List()
          )
        } |+|
        getCreature.takeStaminaDamage(8f)
    else Monoid[GameStateTransition].empty

  def onActiveStart()(implicit gameState: GameState): GameStateTransition

  def onActiveUpdate()(implicit gameState: GameState): GameStateTransition

  def onChannelStart()(implicit gameState: GameState): GameStateTransition

  def onChannelUpdate()(implicit gameState: GameState): GameStateTransition

  def onInactiveStart()(implicit gameState: GameState): GameStateTransition

  def updateHitbox()(implicit gameState: GameState): GameStateTransition = {
    if (state.dirVector.isEmpty)
      throw new RuntimeException("trying to update hitbox without setting dir vector!")

    val dirVector = state.dirVector.get match {
      case dirVector if dirVector.length <= 0 => Vec2(1, 0).normal
      case dirVector                          => dirVector
    }

    val creature = getCreature

    val theta = new Vector2(dirVector.x, dirVector.y).angleDeg()

    val attackShiftX = dirVector.normal.x * attackRange
    val attackShiftY = dirVector.normal.y * attackRange

    val attackRectX = attackShiftX + creature.state.pos.x
    val attackRectY = attackShiftY + creature.state.pos.y

    State { implicit gameState =>
      (
        modifyAbility(
          _.modify(_.state.hitbox).setTo(
            Some(
              Hitbox(
                pos = Vec2(attackRectX, attackRectY),
                width = currentAnimation.width,
                height = currentAnimation.height,
                rotation = theta,
                scale = currentAnimation.scale
              )
            )
          )
        ),
        List()
      )
    }
  }

  def update(delta: Float)(implicit gameState: GameState): GameStateTransition =
    (state.stage match {
      case AbilityStage.InactiveStage =>
        if (state.justPerformed) updateStateToChannel()
        else Monoid[GameStateTransition].empty
      case AbilityStage.ChannelStage =>
        onChannelUpdate() |+|
          (
            if (state.stageTimer.time > getAbility.currentAnimation.channelTime)
              updateStateToActive()
            else
              Monoid[GameStateTransition].empty
          )
      case AbilityStage.ActiveStage =>
        onActiveUpdate() |+| (
          if (state.stageTimer.time > getAbility.currentAnimation.activeTime)
            updateStateToInactive()
          else
            Monoid[GameStateTransition].empty
        )

    }) |+| updateTimers(delta) |+| (if (state.currentAttackPhaseResetTimer.time > currentAttackPhaseResetTimeout)
                                      State { implicit gameState =>
                                        (modifyAbility(_.modify(_.state.currentAttackPhase).setTo(0)), List())
                                      }
                                    else Monoid[GameStateTransition].empty)

  private def updateStateToInactive()(implicit gameState: GameState): GameStateTransition = {
    onInactiveStart() |+|
      State { implicit gameState =>
        (
          modifyAbility(
            _.modify(_.state.stage)
              .setTo(AbilityStage.InactiveStage)
              .modify(_.state.stageTimer)
              .using(_.restart())
              .modify(_.state.currentAttackPhase)
              .setTo((state.currentAttackPhase + 1) % getAbility.attackPhases.length)
          ),
          List(AbilityBodyDeactivateEvent(id))
        )
      }
  }

  private def updateStateToActive()(implicit gameState: GameState): GameStateTransition =
    onActiveStart() |+|
      State { implicit gameState =>
        (
          modifyAbility(
            _.modify(_.state.stage)
              .setTo(AbilityStage.ActiveStage)
              .modify(_.state.stageTimer)
              .using(_.restart())
          ),
          List(AbilityBodyActivateEvent(id)) ++
            (if (currentAttackActiveSoundId.nonEmpty && currentAttackActiveSoundPitch.nonEmpty)
               List(PlaySoundEvent(currentAttackActiveSoundId.get, currentAttackActiveSoundPitch.get))
             else List())
        )
      }

  private def updateStateToChannel()(implicit gameState: GameState): GameStateTransition =
    onChannelStart() |+|
      State { implicit gameState =>
        (
          modifyAbility(
            _.modify(_.state.stage)
              .setTo(AbilityStage.ChannelStage)
              .modify(_.state.stageTimer)
              .using(_.restart())
              .modify(_.state.stageTimer)
              .using(_.restart())
              .modify(_.state.justPerformed)
              .setTo(false)
              .modify(_.state.currentAttackPhaseResetTimer)
              .using(_.restart())
          ),
          List()
        )
      }

  private def updateTimers(delta: Float): GameStateTransition =
    State { implicit gameState =>
      (
        gameState.pipe(
          implicit gameState =>
            modifyAbility(_.modifyAll(_.state.stageTimer, _.state.currentAttackPhaseResetTimer).using(_.update(delta)))
        ),
        List()
      )
    }

  def forceStop(): GameStateTransition = {
    if (state.stage != AbilityStage.InactiveStage)
      State { implicit gameState =>
        (
          gameState.pipe(
            implicit gameState => modifyAbility(_.modify(_.state.stage).setTo(AbilityStage.InactiveStage))
          ),
          List(AbilityBodyDeactivateEvent(id))
        )
      }
    else Monoid[GameStateTransition].empty

  }

  def copy(state: AbilityState = state): Ability
}

object Ability {
  def abilityByName(creatureId: CreatureId, areaId: AreaId, name: String): Ability =
    name match {
      case "slash"        => SlashAbility(name, creatureId, areaId)
      case "triple_slash" => TripleSlashAbility(name, creatureId, areaId)

    }

}
