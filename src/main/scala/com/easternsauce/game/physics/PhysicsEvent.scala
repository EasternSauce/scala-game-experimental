package com.easternsauce.game.physics

import com.easternsauce.model.ids.{AbilityId, CreatureId}

trait PhysicsEvent

case class AbilityCollisionEvent(
  creatureId: CreatureId,
  abilityId: AbilityId,
  collidedCreatureId: CreatureId)
    extends PhysicsEvent
