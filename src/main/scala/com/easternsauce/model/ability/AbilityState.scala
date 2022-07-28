package com.easternsauce.model.ability

import com.easternsauce.model.SimpleTimer
import com.easternsauce.model.ability.AbilityStage.AbilityStage
import com.easternsauce.model.ids.{AbilityId, CreatureId, ProjectileId}

case class AbilityState(
  id: AbilityId,
  creatureId: CreatureId,
  attack: Option[Attack],
  projectiles: Option[List[ProjectileId]],
  stageTimer: SimpleTimer,
  stage: AbilityStage
) {}
