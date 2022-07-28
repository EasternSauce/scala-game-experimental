package com.easternsauce.model.ability

import com.easternsauce.model.SimpleTimer
import com.easternsauce.model.ability.AbilityStage.AbilityStage
import com.easternsauce.model.ids.{AbilityId, CreatureId, ProjectileId}

case class AbilityState(
  id: AbilityId,
  creatureId: CreatureId,
  projectiles: Option[List[ProjectileId]] = None,
  stageTimer: SimpleTimer = SimpleTimer(time = Float.MaxValue),
  stage: AbilityStage = AbilityStage.Inactive,
  attack: Option[Attack] = None
)
