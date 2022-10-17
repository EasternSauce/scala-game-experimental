package com.easternsauce.model.ability

import com.easternsauce.model.SimpleTimer
import com.easternsauce.model.ability.ProjectileStage.ProjectileStage
import com.easternsauce.model.ids.{AbilityId, AreaId, CreatureId, ProjectileId}

case class ProjectileState(
  id: ProjectileId,
  areaId: AreaId,
  creatureId: CreatureId,
  abilityId: AbilityId,
  timer: SimpleTimer = SimpleTimer(),
  channelTime: Float = 0f,
  activeTime: Float = 10f,
  stage: ProjectileStage = ProjectileStage.Channel
) {}

object ProjectileState {
  def apply(ability: Ability, counter: Long): ProjectileState =
    new ProjectileState(
      id = ProjectileId.derive(ability, counter),
      creatureId = ability.state.creatureId,
      areaId = ability.state.areaId,
      abilityId = ability.id
    )
}
