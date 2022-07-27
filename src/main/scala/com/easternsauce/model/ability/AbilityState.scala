package com.easternsauce.model.ability

import com.easternsauce.model.SimpleTimer
import com.easternsauce.model.ability.AbilityStage.AbilityStage
import com.easternsauce.model.ids.{AbilityId, AttackId, CreatureId, ProjectileId}

case class AbilityState(
  id: AbilityId,
  creatureId: CreatureId,
  attack: Option[AttackId],
  projectiles: Option[List[ProjectileId]],
  stageTimer: SimpleTimer,
  stage: AbilityStage
) {}
