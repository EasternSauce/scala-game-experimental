package com.easternsauce.model.creature

import com.easternsauce.model.ids.{AreaId, CreatureId}
import com.easternsauce.model.{SimpleTimer, Vec2}

case class CreatureState(
  id: CreatureId,
  pos: Vec2,
  movingDir: Vec2 = Vec2(0, 0),
  animationTimer: SimpleTimer = SimpleTimer(),
  areaId: AreaId,
  currentSpeed: Float = 0f
) {}
