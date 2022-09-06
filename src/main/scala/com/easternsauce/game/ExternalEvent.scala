package com.easternsauce.game

import com.easternsauce.model.ids.{AbilityId, AreaId, CreatureId}

trait ExternalEvent

case class RenderEvent()                                          extends ExternalEvent
case class AbilityBodyCreateEvent(abilityId: AbilityId)           extends ExternalEvent
case class AbilitySpriteRendererCreateEvent(abilityId: AbilityId) extends ExternalEvent
case class AbilityBodyActivateEvent(abilityId: AbilityId)         extends ExternalEvent
case class AreaRefreshEvent(areaId: AreaId)                       extends ExternalEvent
case class AbilityBodyDestroyEvent(abilityId: AbilityId)          extends ExternalEvent
case class CreatureBodySetSensorEvent(creatureId: CreatureId)     extends ExternalEvent
