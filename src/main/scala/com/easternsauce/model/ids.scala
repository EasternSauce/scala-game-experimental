package com.easternsauce.model

import com.easternsauce.model.ability.Ability

object ids {
  case class CreatureId(value: String)
  case class AbilityId(value: String)
  object AbilityId {
    def derive(creatureId: CreatureId, areaId: AreaId, abilityName: String): AbilityId =
      AbilityId(creatureId.value + "_" + areaId.value + "_" + abilityName)
  }
  case class ProjectileId(value: String)
  object ProjectileId {
    def derive(creatureId: CreatureId, areaId: AreaId, abilityId: AbilityId, counter: Long): ProjectileId =
      ProjectileId(creatureId.value + "_" + areaId.value + "_" + abilityId.value + "_" + counter)
    def derive(ability: Ability, counter: Long): ProjectileId =
      derive(ability.state.creatureId, ability.state.areaId, ability.state.id, counter)
  }
  case class AreaId(value: String)
}
