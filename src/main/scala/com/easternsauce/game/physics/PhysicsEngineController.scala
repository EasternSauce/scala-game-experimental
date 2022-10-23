package com.easternsauce.game.physics

import com.badlogic.gdx.maps.tiled.TiledMap
import com.badlogic.gdx.physics.box2d._
import com.easternsauce.model.GameState
import com.easternsauce.model.GameState.{getAbility, getCreature}
import com.easternsauce.model.ids.{AbilityId, AreaId, CreatureId, ProjectileId}

import scala.collection.mutable.ListBuffer

case object PhysicsEngineController {
  var creatureBodies: Map[CreatureId, CreatureBody] = _
  var abilityBodies: Map[AbilityId, AbilityBody] = _
  var projectileBodies: Map[ProjectileId, ProjectileBody] = _
  var physicsWorlds: Map[AreaId, PhysicsWorld] = _

  var physicsEventQueue: ListBuffer[PhysicsEvent] = _

  def init(maps: Map[AreaId, TiledMap])(implicit gameState: GameState): Unit = {
    this.physicsWorlds = maps.map { case (areaId, map) => areaId -> PhysicsWorld(map) }

    this.physicsEventQueue = ListBuffer()

    physicsWorlds.values.foreach { world =>
      world.init()
      createContactListener(world.b2world)
    }

    creatureBodies = gameState.creatures.keys.map(creatureId => creatureId -> CreatureBody(creatureId)).toMap

    creatureBodies.values.foreach { creatureBody =>
      {
        val destinationAreaId = getCreature(creatureBody.creatureId, gameState).state.areaId
        creatureBody.init(destinationAreaId)
      }

    }

    abilityBodies = Map()
    projectileBodies = Map()
  }

  def addAbilityBody(abilityId: AbilityId)(implicit gameState: GameState): Unit = {
    if (!abilityBodies.contains(abilityId)) {
      val abilityBody = AbilityBody(abilityId)

      abilityBodies = abilityBodies.updated(abilityId, abilityBody)
    }
  }

  def activateAbilityBody(abilityId: AbilityId)(implicit gameState: GameState): Unit = {
    val ability = getAbility(abilityId, gameState)

    if (ability.currentAttackPhase.nonEmpty) {
      abilityBodies(abilityId).init()
      abilityBodies(abilityId).isActive = true
    }
  }

  def deactivateAbilityBody(abilityId: AbilityId)(implicit gameState: GameState): Unit = {
    if (abilityBodies(abilityId).isActive) {
      abilityBodies(abilityId).destroy()
      abilityBodies(abilityId).isActive = false
    }
  }

  def addProjectileBody(projectileId: ProjectileId)(implicit gameState: GameState): Unit = {
    if (!projectileBodies.contains(projectileId)) {
      val projectileBody = ProjectileBody(projectileId)

      projectileBodies = projectileBodies.updated(projectileId, projectileBody)
    }
  }

  def setCreatureBodyToSensor(creatureId: CreatureId): Unit = {
    creatureBodies(creatureId).b2Body.getFixtureList.get(0).setSensor(true)
  }

  def changeCreatureArea(creatureId: CreatureId, fromAreaId: AreaId, toAreaId: AreaId)(implicit
    gameState: GameState
  ): Unit = {
    physicsWorlds(fromAreaId).b2world.destroyBody(creatureBodies(creatureId).b2Body)
    creatureBodies(creatureId).b2Body = null
    creatureBodies(creatureId).init(toAreaId)
  }

  def update()(implicit gameState: GameState): Unit = {
    physicsWorlds.values.foreach(_.step())
//    physicsWorlds(gameState.currentAreaId).step() TODO: should non-active world be updated or not?
    creatureBodies.values.foreach(_.update())
    abilityBodies.values.foreach(_.update())
  }

  def createContactListener(world: World): Unit = {

    val contactListener: ContactListener = new ContactListener {
      override def beginContact(contact: Contact): Unit = {
        val objA = contact.getFixtureA.getBody.getUserData
        val objB = contact.getFixtureB.getBody.getUserData

        def onContactStart(pair: (AnyRef, AnyRef)): Unit =
          pair match { // will run onContact twice for same type objects!
            case (creatureBody: CreatureBody, abilityBody: AbilityBody) =>
              if (creatureBody.creatureId != abilityBody.creatureId)
                physicsEventQueue.prepend(
                  AbilityCollisionEvent(abilityBody.creatureId, abilityBody.abilityId, creatureBody.creatureId)
                )
            case (creatureBody: CreatureBody, areaGateBody: AreaGateBody) =>
              physicsEventQueue.prepend(AreaGateCollisionStartEvent(creatureBody.creatureId, areaGateBody))

            //            case (entityBody: EntityBody, areaGateBody: AreaGateBody) =>
//              physicsEventQueue.prepend(AreaGateCollisionStartEvent(entityBody.creatureId, areaGateBody))
//            case (entityBody: EntityBody, lootPileBody: LootPileBody) =>
//              physicsEventQueue.prepend(
//                LootPileCollisionStartEvent(entityBody.creatureId, lootPileBody.areaId, lootPileBody.lootPileId)
//              )
            case _ =>
          }

        onContactStart(objA, objB)
        onContactStart(objB, objA)
      }

      override def endContact(contact: Contact): Unit = {
        if (contact.getFixtureA != null && contact.getFixtureB != null) {
          val objA = contact.getFixtureA.getBody.getUserData
          val objB = contact.getFixtureB.getBody.getUserData

          def onContactEnd(pair: (AnyRef, AnyRef)): Unit =
            pair match { // will run onContact twice for same type objects!
              case (entityBody: CreatureBody, _: AreaGateBody) =>
                physicsEventQueue.prepend(AreaGateCollisionEndEvent(entityBody.creatureId))
              //            case (entityBody: EntityBody, _: AreaGateBody) =>
              //              physicsEventQueue.prepend(AreaGateCollisionEndEvent(entityBody.creatureId))
              //            case (entityBody: EntityBody, lootPileBody: LootPileBody) =>
              //              physicsEventQueue.prepend(
              //                LootPileCollisionEndEvent(entityBody.creatureId, lootPileBody.areaId, lootPileBody.lootPileId)
              //              )
              case _ =>
            }

          onContactEnd(objA, objB)
          onContactEnd(objB, objA)
        }

      }
      override def preSolve(contact: Contact, oldManifold: Manifold): Unit = {}

      override def postSolve(contact: Contact, impulse: ContactImpulse): Unit = {}
    }

    world.setContactListener(contactListener)
  }

}
