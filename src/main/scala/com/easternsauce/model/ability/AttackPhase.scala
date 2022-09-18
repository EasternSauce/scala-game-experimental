package com.easternsauce.model.ability

case class AttackPhase(
  animation: AbilityAnimationData,
  knockbackVelocity: Float,
  attackActiveSoundId: Option[String],
  attackActiveSoundPitch: Option[Float]
) {}
