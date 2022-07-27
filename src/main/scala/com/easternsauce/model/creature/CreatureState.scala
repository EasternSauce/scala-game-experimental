package com.easternsauce.model.creature

import com.easternsauce.model.ids.{AbilityId, AreaId, CreatureId}
import com.easternsauce.model.{SimpleTimer, Vec2}

case class CreatureState(
  id: CreatureId,
  pos: Vec2,
  movingDir: Vec2,
  animationTimer: SimpleTimer,
  areaId: AreaId,
  currentSpeed: Float,
  actionDirVector: Vec2,
  abilities: List[AbilityId]
)
