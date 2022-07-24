package com.easternsauce.game.physics

import com.badlogic.gdx.physics.box2d.BodyDef.BodyType
import com.badlogic.gdx.physics.box2d._
import com.easternsauce.model.creature.Creature

object B2BodyFactory {
  private def createB2body(
    world: World,
    posX: Float,
    posY: Float,
    bodyType: BodyType,
    userData: AnyRef,
    shape: BodyShape,
    isSensor: Boolean = false,
    sleepingAllowed: Boolean = true,
    linearDamping: Option[Float] = None,
    mass: Option[Float] = None
  ): Body = {
    val bodyDef = new BodyDef()
    bodyDef.`type` = bodyType
    bodyDef.position.set(posX, posY)

    val b2body = world.createBody(bodyDef)

    b2body.setUserData(userData)

    val fixtureDef: FixtureDef = new FixtureDef

    fixtureDef.shape = shape.b2Shape()
    fixtureDef.isSensor = isSensor

    b2body.createFixture(fixtureDef)

    linearDamping match {
      case Some(linearDamping) => b2body.setLinearDamping(linearDamping)
      case _                   =>
    }

    mass match {
      case Some(mass) =>
        val massData = new MassData()
        massData.mass = mass
        b2body.setMassData(massData)
      case _ =>
    }

    b2body

  }

  def createTerrainTileB2body(world: World, terrainTileBody: TerrainTileBody): Body = {
    createB2body(
      world = world,
      posX = terrainTileBody.x * terrainTileBody.tileWidth + terrainTileBody.tileWidth / 2,
      posY = terrainTileBody.y * terrainTileBody.tileHeight + terrainTileBody.tileHeight / 2,
      bodyType = BodyType.StaticBody,
      userData = terrainTileBody,
      shape = Rectangle(terrainTileBody.tileWidth, terrainTileBody.tileHeight)
    )
  }

  def createCreatureB2body(world: World, creatureBody: CreatureBody, creature: Creature): Body = {
    createB2body(
      world = world,
      posX = creature.state.pos.x,
      posY = creature.state.pos.y,
      bodyType = BodyDef.BodyType.DynamicBody,
      userData = creatureBody,
      shape = Circle(creature.width / 2),
      sleepingAllowed = false,
      linearDamping = Some(10f),
      mass = Some(1000f)
    )
  }

}

sealed abstract class BodyShape { def b2Shape(): Shape }

case class Circle(radius: Float) extends BodyShape {
  def b2Shape(): Shape = { val shape = new CircleShape(); shape.setRadius(radius); shape }
}
case class Rectangle(width: Float, height: Float) extends BodyShape {
  def b2Shape(): Shape = { val shape = new PolygonShape(); shape.setAsBox(width / 2, height / 2); shape }
}
case class Polygon(vertices: Array[Float]) extends BodyShape {
  def b2Shape(): Shape = { val shape = new PolygonShape(); shape.set(vertices); shape }
}
