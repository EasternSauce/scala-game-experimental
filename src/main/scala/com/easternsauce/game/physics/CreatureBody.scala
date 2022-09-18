package com.easternsauce.game.physics

import com.badlogic.gdx.math.Vector2
import com.badlogic.gdx.physics.box2d.Body
import com.easternsauce.model.GameState
import com.easternsauce.model.GameState.getCreature
import com.easternsauce.model.ids.CreatureId

case class CreatureBody(creatureId: CreatureId) {
  var b2Body: Body = _

  def init()(implicit gameState: GameState): Unit = {
    implicit val _creatureId: CreatureId = creatureId
    val physicsWorld = PhysicsEngineController.physicsWorlds(getCreature.state.areaId)

    b2Body =
      B2BodyFactory.createCreatureB2body(world = physicsWorld.b2world, creatureBody = this, creature = getCreature)

    if (!getCreature.isAlive) b2Body.getFixtureList.get(0).setSensor(true)
  }

  def update()(implicit gameState: GameState): Unit = {

    val creature = gameState.creatures(creatureId)

    val bodyCreated = PhysicsEngineController.creatureBodies.contains(creatureId)

    val v = creature.state.currentSpeed
    val normalMovingDir = creature.state.movingDir.normal
    val vectorX = normalMovingDir.x * v
    val vectorY = normalMovingDir.y * v

    if (bodyCreated) {
      if (creature.isEffectActive("knockback")) {
        PhysicsEngineController
          .creatureBodies(creatureId)
          .setVelocity(
            new Vector2(
              creature.state.knockbackDir.x * creature.state.knockbackVelocity,
              creature.state.knockbackDir.y * creature.state.knockbackVelocity
            )
          )
      } else if (creature.ableToMove)
        setVelocity(new Vector2(vectorX, vectorY))
    }
  }

  def pos: Vector2 = b2Body.getWorldCenter

  def setVelocity(velocity: Vector2): Unit = b2Body.setLinearVelocity(velocity)

}
