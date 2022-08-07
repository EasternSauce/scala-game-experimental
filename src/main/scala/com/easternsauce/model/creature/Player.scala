package com.easternsauce.model.creature

import com.easternsauce.model.WorldDirection.WorldDirection
import com.easternsauce.model.ids.{AreaId, CreatureId}
import com.easternsauce.model.{SimpleTimer, Vec2, WorldDirection}

case class Player(state: CreatureState) extends Creature {
  override val textureName: String = "male1"
  override val textureWidth: Int = 32
  override val textureHeight: Int = 32
  override val width: Float = 2
  override val height: Float = 2
  override val frameDuration: Float = 0.1f
  override val frameCount: Int = 3
  override val neutralStanceFrame: Int = 1
  override val dirMap: Map[WorldDirection, Int] =
    Map(WorldDirection.Up -> 3, WorldDirection.Down -> 0, WorldDirection.Left -> 1, WorldDirection.Right -> 2)

  override val abilityNames: List[String] = List("slash")

  override def copy(state: CreatureState = state): Creature = Player(state)
}

object Player {
  def apply(id: CreatureId, areaId: AreaId, pos: Vec2): Creature = {
    new Player(
      CreatureState(
        id = id,
        pos = pos,
        areaId = areaId
      )
    )
  }
}
