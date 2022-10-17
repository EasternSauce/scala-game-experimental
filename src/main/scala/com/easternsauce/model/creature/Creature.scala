package com.easternsauce.model.creature

import cats.Monoid
import cats.data.State
import cats.implicits.{catsSyntaxSemigroup, toFoldableOps}
import com.easternsauce.game._
import com.easternsauce.model.GameState.{GameStateTransition, gameStateMonoid, getAbilitiesOfCreature, getAbility, getCreature, modifyCreature}
import com.easternsauce.model.WorldDirection.WorldDirection
import com.easternsauce.model.ability.Ability
import com.easternsauce.model.ids.{AbilityId, AreaId, CreatureId}
import com.easternsauce.model.{GameState, Vec2, WorldDirection}
import com.softwaremill.quicklens._

import scala.language.postfixOps
import scala.util.chaining.scalaUtilChainingOps

trait Creature {

  val state: CreatureState

  val textureName: String
  val textureWidth: Int
  val textureHeight: Int
  val width: Float
  val height: Float
  val frameDuration: Float
  val frameCount: Int
  val neutralStanceFrame: Int
  val dirMap: Map[WorldDirection, Int]

  val speed: Float = 10f

  val abilityNames: List[String] = List()

  val defaultAbilityName = "slash"

  val abilityUsages: Map[String, AbilityUsage] = Map()

  val useAbilityTimeout: Float = 4

  val staminaRegenerationTickTime = 0.02f
  val staminaRegeneration = 0.8f
  val staminaOveruseTime = 2.8f
  val staminaRegenerationDisabledTime = 1.7f

  val onGettingHitSoundId: Option[String] = None

  implicit val id: CreatureId = state.id

  def isMoving: Boolean = state.currentSpeed > 0f

  def isPlayer: Boolean = false

  def isEnemy: Boolean = false

  def isControlledAutomatically: Boolean = false

  def facingDirection: WorldDirection =
    state.movingDir.angleDeg() match {
      case angle if angle >= 45 && angle < 135  => WorldDirection.Up
      case angle if angle >= 135 && angle < 225 => WorldDirection.Left
      case angle if angle >= 225 && angle < 315 => WorldDirection.Down
      case _                                    => WorldDirection.Right
    }

  def moveInDir(dir: Vec2)(implicit gameState: GameState): GameStateTransition =
    State[GameState, List[ExternalEvent]] { implicit gameState =>
      (modifyCreature(_.modify(_.state.movingDir).setTo(dir)), List())
    }

  def startMoving()(implicit gameState: GameState): GameStateTransition =
    State { implicit gameState =>
      (
        modifyCreature(
          _.modify(_.state.currentSpeed)
            .setTo(this.speed)
            .modify(_.state.animationTimer)
            .using(_.restart())
        ),
        List()
      )
    }

  def stopMoving()(implicit gameState: GameState): GameStateTransition =
    State { implicit gameState =>
      (modifyCreature(_.modify(_.state.currentSpeed).setTo(0f)), List())
    }

  def isAlive: Boolean = state.life > 0f

  def ableToMove: Boolean = !this.isEffectActive("stagger") && !this.isEffectActive("knockback") && this.isAlive

  def onDeath()(implicit gameState: GameState): GameStateTransition =
    State[GameState, List[ExternalEvent]](
      implicit gameState => (modifyCreature(_.modify(_.state.stamina).setTo(0f)), List(CreatureBodySetSensorEvent(id)))
    ) |+| getAbilitiesOfCreature.values.toList.foldMap(_.forceStop())

  def update(delta: Float)(implicit gameState: GameState): GameStateTransition = {
//    println("pos " + state.pos)
    updateTimers(delta) |+|
      updateStamina(delta) |+|
      (if (isControlledAutomatically) updateAutomaticControls()
       else Monoid[GameStateTransition].empty) |+|
      state.events.foldMap { case CreatureDeathEvent() => onDeath() } |+|
      State(implicit gameState => (modifyCreature(_.modify(_.state.events).setTo(List())), List())) |+|
      State(
        implicit gameState =>
          (
            modifyCreature(_.modify(_.state.effects).using(_.map {
              case (name, effect) => (name, effect.update(delta))
            })),
            List()
          )
      )
  }

  def activateEffect(effect: String, time: Float): GameStateTransition =
    if (state.effects.contains(effect))
      State { implicit gameState =>
        (modifyCreature(_.modify(_.state.effects.at(effect)).using(_.activate(time))), List())
      }
    else
      State { implicit gameState =>
        (
          modifyCreature(
            creature =>
              creature.modify(_.state.effects).setTo(creature.state.effects + (effect -> Effect(effect).activate(time)))
          ),
          List()
        )
      }

  def isEffectActive(effect: String): Boolean =
    state.effects.contains(effect) && state.effects(effect).isActive

  def updateTimers(delta: Float)(implicit gameState: GameState): GameStateTransition =
    State { implicit gameState =>
      (
        modifyCreature(
          _.modifyAll(
            _.state.animationTimer,
            _.state.pathCalculationCooldownTimer,
            _.state.useAbilityTimer,
            _.state.staminaRegenerationDisabledTimer,
            _.state.staminaOveruseTimer,
            _.state.staminaDrainTimer,
            _.state.staminaRegenerationTimer
          ).using(_.update(delta))
        ),
        List()
      )
    }

  def updateAutomaticControls()(implicit gameState: GameState): GameStateTransition =
    Monoid[GameStateTransition].empty

  def attack(dir: Vec2)(implicit gameState: GameState): GameStateTransition = {
    implicit val abilityId: AbilityId = AbilityId.derive(id, state.areaId, defaultAbilityName)
    if (getCreature.abilityNames.contains(defaultAbilityName))
      getAbility.perform(dir)
    else Monoid[GameStateTransition].empty
  }

  def takeLifeDamage(damage: Float, sourcePosX: Float, sourcePosY: Float, knockbackVelocity: Float)(implicit
    gameState: GameState
  ): GameStateTransition = {
    val beforeLife = getCreature.state.life

    val actualDamage = damage * 100f / (100f + state.totalArmor)

    //      .usingIf(creature.onGettingHitSoundId.nonEmpty)(_.prepended(PlaySoundEvent(creature.onGettingHitSoundId.get)))

    val handleTakeDamage = State[GameState, List[ExternalEvent]] { implicit gameState: GameState =>
      (
        modifyCreature(
          _.pipe(
            creature =>
              if (creature.state.life - actualDamage > 0)
                creature.modify(_.state.life).setTo(creature.state.life - actualDamage)
              else creature.modify(_.state.life).setTo(0f).modify(_.state.isDead).setTo(true)
          )
        ).pipe(
          implicit gameState =>
            if (beforeLife > 0f && getCreature.state.life <= 0f)
              modifyCreature(_.modify(_.state.events).using(_.appended(CreatureDeathEvent())))
            else gameState
        ),
        if (getCreature.onGettingHitSoundId.nonEmpty)
          List(PlaySoundWithRandomPitchEvent(getCreature.onGettingHitSoundId.get))
        else List()
      )
    }

    val handleKnockback = getCreature.activateEffect("knockback", 0.05f) |+|
      State[GameState, List[ExternalEvent]] { implicit gameState: GameState =>
        (
          modifyCreature(
            _.modify(_.state.knockbackDir)
              .setTo(Vec2(getCreature.state.pos.x - sourcePosX, getCreature.state.pos.y - sourcePosY).normal)
              .modify(_.state.knockbackVelocity)
              .setTo(knockbackVelocity)
          ),
          List()
        )
      }

    handleTakeDamage |+| handleKnockback

  }

  def takeStaminaDamage(damage: Float): GameStateTransition =
    State { implicit gameState =>
      (
        if (state.stamina - damage > 0) modifyCreature(_.modify(_.state.stamina).setTo(state.stamina - damage))
        else {
          modifyCreature(
            _.modify(_.state.stamina)
              .setTo(0f)
              .modify(_.state.staminaOveruse)
              .setTo(true)
              .modify(_.state.staminaOveruseTimer)
              .using(_.restart())
          )
        },
        List()
      )
    }

  def updateStamina(delta: Float): GameStateTransition = {
    State { implicit gameState =>
      (
        gameState
          .pipe(
            implicit gameState =>
              if (state.isSprinting && state.stamina > 0)
                modifyCreature(_.modify(_.state.staminaDrainTimer).using(_.update(delta)))
              else gameState
          )
          .pipe(
            implicit gameState =>
              if (isAlive && !state.isStaminaRegenerationDisabled && !state.isSprinting)
                modifyCreature(
                  creature =>
                    if (
                      creature.state.staminaRegenerationTimer.time > creature.staminaRegenerationTickTime /* && !abilityActive */ && !creature.state.staminaOveruse
                    ) {
                      creature
                        .pipe(creature => {
                          val afterRegeneration = creature.state.stamina + creature.staminaRegeneration
                          creature
                            .modify(_.state.stamina)
                            .setToIf(creature.state.stamina < creature.state.maxStamina)(
                              Math.min(afterRegeneration, creature.state.maxStamina)
                            )
                        })
                        .modify(_.state.staminaRegenerationTimer)
                        .using(_.restart())
                    } else creature
                )
              else gameState
          )
          .pipe(
            implicit gameState =>
              modifyCreature(
                creature =>
                  creature
                    .modify(_.state.staminaOveruse)
                    .setToIf(
                      creature.state.staminaOveruse && creature.state.staminaOveruseTimer.time > creature.staminaOveruseTime
                    )(false)
                    .modify(_.state.isStaminaRegenerationDisabled)
                    .setToIf(state.staminaRegenerationDisabledTimer.time > staminaRegenerationDisabledTime)(false)
              )
          ),
        List()
      )
    }
  }

  def init()(implicit gameState: GameState): GameStateTransition = {
    val areaId = state.areaId
    initAbilities(areaId)
  }

  private def initAbilities(areaId: AreaId): GameStateTransition = {
    State { implicit gameState =>
      (
        abilityNames.foldLeft(gameState) {
          case (gameState, abilityName) =>
            val ability = Ability.abilityByName(id, areaId, abilityName)
            gameState.modify(_.abilities).using(_.updated(ability.id, ability))
        }, {

          abilityNames.flatMap(abilityName => {
            val abilityId = AbilityId.derive(id, areaId, abilityName)
            List(AbilityBodyCreateEvent(abilityId), AbilitySpriteRendererCreateEvent(abilityId))
          })

        }
      )
    }
  }

  def changeArea(oldAreaId: Option[AreaId], newAreaId: AreaId)(implicit gameState: GameState): GameStateTransition = {

    getAbilitiesOfCreature.values.toList.foldMap(_.forceStop()) |+|
      State[GameState, List[ExternalEvent]] { implicit gameState =>
        (
          if (oldAreaId.nonEmpty) {
            modifyCreature {
              _.modify(_.state.areaId).setTo(newAreaId)
            } // set creature area id to new area id
              .modify(_.areas.at(oldAreaId.get).state.creatures)
              .using(_.filter(_ != state.id)) // remove creature id from old area
              .modify(_.areas.at(newAreaId).state.creatures)
              .using(state.id :: _) // add creature id to new area
          } else {
            modifyCreature {
              _.modify(_.state.areaId).setTo(newAreaId)
            } // set creature area id to new area id
              .modify(_.areas.at(newAreaId).state.creatures)
              .using(state.id :: _) // add creature id to new area
          },
          List()
        )
      } |+| initAbilities(newAreaId)

  }

  def setPosition(newPosX: Float, newPosY: Float): GameStateTransition = {
    State[GameState, List[ExternalEvent]] { implicit gameState =>
      (
        modifyCreature {
          _.modify(_.state.pos.x)
            .setTo(newPosX)
            .modify(_.state.pos.y)
            .setTo(newPosY)
        },
        List()
      )
    }

  }

  def capability: Int =
    if (width >= 0 && width < 2) 1
    else if (width >= 2 && width <= 4) 2
    else if (width >= 4 && width <= 6) 3
    else 4

  def copy(state: CreatureState): Creature
}

case class AbilityUsage(
  weight: Float,
  minimumDistance: Float = 0f,
  maximumDistance: Float = Float.MaxValue,
  lifeThreshold: Float = 1.0f
)
