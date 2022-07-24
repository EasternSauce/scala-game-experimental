package game.physics

import com.badlogic.gdx.math.Vector2
import com.badlogic.gdx.physics.box2d.Body
import model.GameState

case class CreatureBody(creatureId: String) {
  var b2Body: Body = _

  def init(gameState: GameState, physicsWorld: PhysicsWorld): Unit = {
    val creature = gameState.creatures(creatureId)

    b2Body = B2BodyFactory.createCreatureB2body(world = physicsWorld.b2world, creatureBody = this, creature = creature)

    if (!creature.isAlive) b2Body.getFixtureList.get(0).setSensor(true)
  }

  def update(gameState: GameState): Unit = {

    val creature = gameState.creatures(creatureId)

    val bodyCreated = PhysicsEngineController.creatureBodies.contains(creatureId)

    val v = creature.state.currentSpeed
    val normalMovingDir = creature.state.movingDir.normal
    val vectorX = normalMovingDir.x * v
    val vectorY = normalMovingDir.y * v

    if (bodyCreated) {
      setVelocity(new Vector2(vectorX, vectorY))
    }
  }

  def pos: Vector2 = b2Body.getWorldCenter

  def setVelocity(velocity: Vector2): Unit = b2Body.setLinearVelocity(velocity)

}
