package game.physics

import com.badlogic.gdx.maps.tiled.TiledMap
import com.badlogic.gdx.physics.box2d.World
import model.GameState

case object PhysicsEngineController {
  var creatureBodies: Map[String, CreatureBody] = _
  var physicsWorlds: Map[String, PhysicsWorld] = _

  def init(gameState: GameState, maps: Map[String, TiledMap]): Unit = {
    this.physicsWorlds = maps.map { case (areaId, map) => areaId -> PhysicsWorld(map) }

    physicsWorlds.values.foreach(world => {
      world.init()
      createContactListener(world.b2world)
    })

    creatureBodies = gameState.creatures.keys.map(creatureId => creatureId -> CreatureBody(creatureId)).toMap

    creatureBodies.values.foreach(creatureBody => {

      val areaId = gameState.creatures(creatureBody.creatureId).state.areaId

      creatureBody.init(gameState = gameState, physicsWorld = physicsWorlds(areaId))
    })

  }

  def update(gameState: GameState): Unit = {}

  def createContactListener(world: World): Unit = {}
}
