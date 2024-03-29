package com.easternsauce.game

import com.easternsauce.model.ids.{AbilityId, AreaId, CreatureId, ProjectileId}

trait ExternalEvent

case class RenderEvent() extends ExternalEvent
case class AbilityBodyCreateEvent(abilityId: AbilityId) extends ExternalEvent
case class AbilitySpriteRendererCreateEvent(abilityId: AbilityId) extends ExternalEvent
case class AbilityBodyActivateEvent(abilityId: AbilityId) extends ExternalEvent
case class AreaRefreshEvent(areaId: AreaId) extends ExternalEvent
case class AbilityBodyDeactivateEvent(abilityId: AbilityId) extends ExternalEvent
case class CreatureBodySetSensorEvent(creatureId: CreatureId) extends ExternalEvent
case class SoundPlayEvent(soundId: String, pitch: Float) extends ExternalEvent
case class SoundPlayWithRandomPitchEvent(soundId: String) extends ExternalEvent
case class AreaChangeEvent(creatureId: CreatureId, fromAreaId: AreaId, toAreaId: AreaId, posX: Float, posY: Float)
    extends ExternalEvent

case class ProjectileSpawnEvent(projectileId: ProjectileId) extends ExternalEvent
