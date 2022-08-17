package com.easternsauce.model.creature

import com.easternsauce.model.WorldDirection.WorldDirection
import com.easternsauce.model.ids.{AreaId, CreatureId}
import com.easternsauce.model.{Vec2, WorldDirection}

case class Skeleton(state: CreatureState) extends Enemy {
  override val textureName: String     = "skeleton"
  override val textureWidth: Int       = 64
  override val textureHeight: Int      = 64
  override val width: Float            = 2
  override val height: Float           = 2
  override val frameDuration: Float    = 0.05f
  override val frameCount: Int         = 9
  override val neutralStanceFrame: Int = 0
  override val dirMap: Map[WorldDirection, Int] =
    Map(WorldDirection.Up -> 0, WorldDirection.Down -> 2, WorldDirection.Left -> 1, WorldDirection.Right -> 3)

  override val abilityNames: List[String] = List("slash")

  override def copy(state: CreatureState = state): Creature = Skeleton(state)
}

object Skeleton {
  def apply(
    id: CreatureId,
    areaId: AreaId,
    pos: Vec2
  ): Creature =
    new Skeleton(CreatureState(id = id, pos = pos, areaId = areaId))
}
