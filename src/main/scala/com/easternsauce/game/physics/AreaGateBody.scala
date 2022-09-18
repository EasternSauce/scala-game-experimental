package com.easternsauce.game.physics

import com.badlogic.gdx.physics.box2d.{Body, World}
import com.easternsauce.model.ids.AreaId

case class AreaGateBody(area1Id: AreaId, x1: Float, y1: Float, area2Id: AreaId, x2: Float, y2: Float) {

  val width = 1.5f
  val height = 1.5f

  private var entryPoint1Body: Body = _
  private var entryPoint2Body: Body = _

  def init(physicsWorlds: Map[AreaId, PhysicsWorld]): Unit = {
    entryPoint1Body = initEntryPointBody(physicsWorlds(area1Id).b2world, x1, y1, width, height)
    entryPoint2Body = initEntryPointBody(physicsWorlds(area2Id).b2world, x2, y2, width, height)
  }

  def destroy(): Unit = {
    entryPoint1Body.getWorld.destroyBody(entryPoint1Body)
    entryPoint2Body.getWorld.destroyBody(entryPoint2Body)
    entryPoint1Body = null
    entryPoint2Body = null
  }

  def initEntryPointBody(world: World, x: Float, y: Float, width: Float, height: Float): Body = {
    B2BodyFactory.createAreaGateEntryPointB2body(
      world = world,
      areaGateBody = this,
      posX = x,
      posY = y,
      width = width,
      height = height
    )
  }

}
