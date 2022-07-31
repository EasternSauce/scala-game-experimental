package com.easternsauce.game.physics

import com.badlogic.gdx.maps.tiled.TiledMap
import com.badlogic.gdx.physics.box2d.World
import com.easternsauce.model.GameState
import com.easternsauce.model.ids.{AbilityId, AreaId, CreatureId}

case object PhysicsEngineController {
  var creatureBodies: Map[CreatureId, CreatureBody] = _
  var abilityBodies: Map[AbilityId, AbilityBody] = _
  var physicsWorlds: Map[AreaId, PhysicsWorld] = _

  def init(maps: Map[AreaId, TiledMap])(implicit gameState: GameState): Unit = {
    this.physicsWorlds = maps.map { case (areaId, map) => areaId -> PhysicsWorld(map) }

    physicsWorlds.values.foreach(world => {
      world.init()
      createContactListener(world.b2world)
    })

    creatureBodies = gameState.creatures.keys.map(creatureId => creatureId -> CreatureBody(creatureId)).toMap

    creatureBodies.values.foreach(creatureBody => {

      creatureBody.init()
    })

    abilityBodies = Map()

  }

  def addAbilityBody(abilityId: AbilityId)(implicit gameState: GameState): Unit = {
    val abilityBody = AbilityBody(abilityId)

    abilityBodies = abilityBodies.updated(abilityId, abilityBody)
  }

  def activateAbilityBody(abilityId: AbilityId)(implicit gameState: GameState): Unit = {
    abilityBodies(abilityId).init()
    abilityBodies(abilityId).isActive = true
  }
  def destroyAbilityBody(abilityId: AbilityId)(implicit gameState: GameState): Unit = {
    abilityBodies(abilityId).destroy()
    abilityBodies(abilityId).isActive = false
  }

  def update()(implicit gameState: GameState): Unit = {
    physicsWorlds(gameState.currentAreaId).step()
    creatureBodies.values.foreach(_.update())
    abilityBodies.values.foreach(_.update())
  }

  def createContactListener(world: World): Unit = {}
}
