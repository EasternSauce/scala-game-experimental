package com.easternsauce.model

object ids {
  case class CreatureId(value: String)
  case class AbilityId(value: String)
  object AbilityId {
    def derive(creatureId: CreatureId, abilityName: String): AbilityId =
      AbilityId(creatureId.value + "_" + abilityName)
  }
  case class ProjectileId(value: String)
  case class AreaId(value: String)
}
