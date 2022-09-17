package com.easternsauce.model.ability

import com.easternsauce.model.ability.AbilityStage.AbilityStage
import com.easternsauce.model.ids.{AbilityId, CreatureId, ProjectileId}
import com.easternsauce.model.{SimpleTimer, Vec2}

case class AbilityState(
  id: AbilityId,
  creatureId: CreatureId,
  projectiles: Option[List[ProjectileId]] = None,
  stageTimer: SimpleTimer = SimpleTimer(time = Float.MaxValue),
  stage: AbilityStage = AbilityStage.InactiveStage,
  dirVector: Option[Vec2] = None,
  hitbox: Option[Hitbox] = None,
  justPerformed: Boolean = false,
  currentAttackPhase: Int = 0,
  currentAttackPhaseResetTimer: SimpleTimer = SimpleTimer()
)
