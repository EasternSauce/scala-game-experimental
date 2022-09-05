package com.easternsauce.game.physics

import com.badlogic.gdx.graphics.g2d.Sprite
import com.badlogic.gdx.physics.box2d.Body
import com.easternsauce.model.GameState
import com.easternsauce.model.ids.{AbilityId, CreatureId}

case class AbilityBody(
  creatureId: CreatureId,
  abilityId: AbilityId) {
  var b2Body: Body = _

  private val sprite = new Sprite()
  var isActive       = false

  def hitboxVertices()(implicit gameState: GameState): Array[Float] = {
    val ability = gameState.abilities(abilityId)

    val hitbox = ability.state.hitbox.get

    sprite.setSize(hitbox.width, hitbox.height)
    sprite.setCenter(0, 0)
    sprite.setOriginCenter()
    sprite.setRotation(hitbox.rotation)
    sprite.setScale(hitbox.scale)

    val vertices = sprite.getVertices
    Array(
      vertices(0), // take only coordinate sprite vertices
      vertices(1),
      vertices(5),
      vertices(6),
      vertices(10),
      vertices(11),
      vertices(15),
      vertices(16)
    )
  }

  def init()(implicit gameState: GameState): Unit = {
    val ability = gameState.abilities(abilityId)

    val vertices = hitboxVertices()
    b2Body = B2BodyFactory.createAbilityB2body(
      world =
        PhysicsEngineController.physicsWorlds(gameState.currentAreaId).b2world, // should we always take current area?
      abilityBody = this,
      posX = ability.state.hitbox.get.pos.x,
      posY = ability.state.hitbox.get.pos.y,
      vertices = vertices
    )

  }

  def update()(implicit gameState: GameState): Unit = {
    PhysicsEngineController.physicsWorlds(gameState.currentAreaId).b2world
//
//    if (gameState.events.contains(UpdatePhysicsOnCreatureDeathEvent(creatureId))) {
//      b2Body.getFixtureList.get(0).setSensor(true)
//    }

    val ability = gameState.abilities(abilityId)

//    if (
//      isActive
//        && gameState.events.contains(
//        UpdatePhysicsOnComponentDestroyBodyEvent(creatureId, abilityId, componentId)
//      )
//    ) {
//      destroy()
//      isActive = false
//    }

    if (isActive)
      if (ability.state.hitbox.nonEmpty)
        b2Body.setTransform(ability.state.hitbox.get.pos.x, ability.state.hitbox.get.pos.y, 0f)
    //      else if (component.specification.componentType == ComponentType.RangedProjectile) {
//        b2Body.setLinearVelocity(
//          component.params.dirVector.x * component.speed,
//          component.params.dirVector.y * component.speed
//        )
//      }

  }

  def destroy()(implicit gameState: GameState): Unit = {
    PhysicsEngineController.physicsWorlds(gameState.currentAreaId).b2world.destroyBody(b2Body)
    b2Body = null
  }
}
