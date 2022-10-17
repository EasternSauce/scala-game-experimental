package com.easternsauce.model.ability

import com.easternsauce.model.ability.AbilityStage.AbilityStage
import com.easternsauce.model.ids.{AbilityId, AreaId, CreatureId}
import com.easternsauce.model.{SimpleTimer, Vec2}

case class AbilityState(
  id: AbilityId,
  areaId: AreaId,
  creatureId: CreatureId,
  stageTimer: SimpleTimer = SimpleTimer(time = Float.MaxValue),
  stage: AbilityStage = AbilityStage.InactiveStage,
  dirVector: Option[Vec2] = None,
  hitbox: Option[Hitbox] = None,
  justPerformed: Boolean = false,
  currentAttackPhase: Int = 0,
  currentAttackPhaseResetTimer: SimpleTimer = SimpleTimer(),
  projectileCounter: Long = 0
)

object AbilityState {
  def apply(name: String, creatureId: CreatureId, areaId: AreaId): AbilityState =
    new AbilityState(id = AbilityId.derive(creatureId, areaId, name), creatureId = creatureId, areaId = areaId)
}
