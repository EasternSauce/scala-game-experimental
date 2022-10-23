package com.easternsauce.game.physics

import com.badlogic.gdx.math.Vector2
import com.badlogic.gdx.physics.box2d.Body
import com.easternsauce.model.GameState
import com.easternsauce.model.GameState.getProjectile
import com.easternsauce.model.ids.{AreaId, ProjectileId}

case class ProjectileBody(projectileId: ProjectileId) {
  var b2Body: Body = _

  def init(destinationAreaId: AreaId)(implicit gameState: GameState): Unit = {
    implicit val _projectileId: ProjectileId = projectileId
    val physicsWorld = PhysicsEngineController.physicsWorlds(destinationAreaId)

    b2Body = B2BodyFactory.createProjectileB2Body(
      world = physicsWorld.b2world,
      projectileBody = this,
      getProjectile.state.pos.x,
      getProjectile.state.pos.y,
      getProjectile.width
    )

  }

  def update()(implicit gameState: GameState): Unit = {}

  def pos: Vector2 = b2Body.getWorldCenter

  def setVelocity(velocity: Vector2): Unit = b2Body.setLinearVelocity(velocity)

}
