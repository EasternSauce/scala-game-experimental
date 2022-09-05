package com.easternsauce.model.creature

import com.easternsauce.model.ids.{AreaId, CreatureId}
import com.easternsauce.model.{SimpleTimer, Vec2}

case class CreatureState(
  id: CreatureId,
  pos: Vec2,
  movingDir: Vec2 = Vec2(0, 0),
  animationTimer: SimpleTimer = SimpleTimer(),
  areaId: AreaId,
  currentSpeed: Float = 0f,
  actionDirVector: Vec2 = Vec2(0, 0),
  targetCreatureId: Option[CreatureId] = None,
  forcePathCalculation: Boolean = false,
  pathCalculationCooldownTimer: SimpleTimer = SimpleTimer(),
  pathTowardsTarget: Option[List[Vec2]] = None,
  life: Float = 100f,
  maxLife: Float = 100f,
  useAbilityTimer: SimpleTimer = SimpleTimer(time = 4f, isRunning = true),
  inbetweenAbilitiesTime: Float = 0f,
  totalArmor: Float = 0f,
  isDead: Boolean = false,
  events: List[CreatureEvent] = List())
