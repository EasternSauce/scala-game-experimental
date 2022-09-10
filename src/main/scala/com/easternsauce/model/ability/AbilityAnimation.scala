package com.easternsauce.model.ability

trait AbilityAnimation {
  val textureWidth: Int
  val textureHeight: Int
  val activeTime: Float
  val channelTime: Float
  val channelSpriteType: String
  val activeSpriteType: String
  val channelFrameCount: Int
  val activeFrameCount: Int
  val channelFrameDuration: Float
  val activeFrameDuration: Float

  val scale: Float
}

object SlashAnimation extends AbilityAnimation {
  val textureWidth: Int = 40
  val textureHeight: Int = 40
  val activeTime: Float = 0.3f
  val channelTime: Float = 0.3f
  val channelSpriteType: String = "slash_windup2"
  val activeSpriteType: String = "slash2"
  val channelFrameCount: Int = 6
  val activeFrameCount: Int = 6
  val channelFrameDuration: Float = 0.05f
  val activeFrameDuration: Float = 0.05f

  override val scale: Float = 1.4f
}