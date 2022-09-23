package com.easternsauce.game.physics

import com.easternsauce.model.ids.{AbilityId, CreatureId}

trait PhysicsEvent

case class AbilityCollisionEvent(creatureId: CreatureId, abilityId: AbilityId, collidedCreatureId: CreatureId)
    extends PhysicsEvent
case class AreaGateCollisionStartEvent(creatureId: CreatureId, areaGate: AreaGateBody) extends PhysicsEvent
case class AreaGateCollisionEndEvent(creatureId: CreatureId) extends PhysicsEvent
