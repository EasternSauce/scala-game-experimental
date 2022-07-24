package com.easternsauce.model

case class CreatureState(
  id: String,
  pos: Vec2,
  movingDir: Vec2 = Vec2(0, 0),
  animationTimer: SimpleTimer = SimpleTimer(),
  areaId: String,
  currentSpeed: Float = 0f
) {}
