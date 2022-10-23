package com.easternsauce.model.ability

import com.easternsauce.model.ability.ProjectileStage.ProjectileStage
import com.easternsauce.model.ids.{AbilityId, AreaId, CreatureId, ProjectileId}
import com.easternsauce.model.{SimpleTimer, Vec2}

case class ProjectileState(
  id: ProjectileId,
  pos: Vec2,
  areaId: AreaId,
  creatureId: CreatureId,
  abilityId: AbilityId,
  timer: SimpleTimer = SimpleTimer(),
  channelTime: Float = 0f,
  activeTime: Float = 10f,
  stage: ProjectileStage = ProjectileStage.Channel,
  rotation: Float = 0f
)

object ProjectileState {
  def apply(ability: Ability, pos: Vec2, counter: Long): ProjectileState =
    new ProjectileState(
      id = ProjectileId.derive(ability, counter),
      creatureId = ability.state.creatureId,
      areaId = ability.state.areaId,
      abilityId = ability.id,
      pos = pos
    )
}
