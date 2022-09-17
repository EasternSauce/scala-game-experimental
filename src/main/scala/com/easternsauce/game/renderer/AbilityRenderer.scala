package com.easternsauce.game.renderer

import com.badlogic.gdx.graphics.g2d._
import com.easternsauce.game.DrawingLayer
import com.easternsauce.model.GameState
import com.easternsauce.model.GameState.getAbility
import com.easternsauce.model.ability.AbilityStage
import com.easternsauce.model.ids.AbilityId

case class AbilityRenderer(abilityId: AbilityId) {
  implicit val _abilityId: AbilityId = abilityId

  var sprite: Sprite = _

  var channelAnimations: List[Animation[TextureRegion]] = List()
  var activeAnimations: List[Animation[TextureRegion]] = List()

  var channelTextureRegions: List[TextureRegion] = List()
  var activeTextureRegions: List[TextureRegion] = List()

  def init(atlas: TextureAtlas)(implicit gameState: GameState): Unit = {
    sprite = new Sprite()

    for (i <- getAbility.attackPhases.indices) {
      val animation = getAbility.attackPhases(i).animation

      val channelTextureRegion = atlas.findRegion(animation.channelSpriteType)
      val activeTextureRegion = atlas.findRegion(animation.activeSpriteType)

      if (channelTextureRegion == null) throw new RuntimeException("region missing for " + animation.channelSpriteType)
      if (activeTextureRegion == null) throw new RuntimeException("region missing for " + animation.activeSpriteType)

      val channelFrames =
        for { i <- (0 until animation.channelFrameCount).toArray } yield new TextureRegion(
          channelTextureRegion,
          i * animation.textureWidth,
          0,
          animation.textureWidth,
          animation.textureHeight
        )
      val channelAnimation = new Animation[TextureRegion](animation.channelFrameDuration, channelFrames: _*)

      val activeFrames =
        for { i <- (0 until animation.activeFrameCount).toArray } yield new TextureRegion(
          activeTextureRegion,
          i * animation.textureWidth,
          0,
          animation.textureWidth,
          animation.textureHeight
        )
      val activeAnimation = new Animation[TextureRegion](animation.activeFrameDuration, activeFrames: _*)

      channelTextureRegions = channelTextureRegions ++ List(channelTextureRegion)
      activeTextureRegions = activeTextureRegions ++ List(activeTextureRegion)
      channelAnimations = channelAnimations ++ List(channelAnimation)
      activeAnimations = activeAnimations ++ List(activeAnimation)
    }

  }

  def update()(implicit gameState: GameState): Unit = {

    def updateSprite(texture: TextureRegion): Unit =
      if (getAbility.state.hitbox.nonEmpty) {
        val hitbox = getAbility.state.hitbox.get

        sprite.setRegion(texture)
        sprite.setSize(hitbox.width, hitbox.height)
        sprite.setCenter(hitbox.pos.x, hitbox.pos.y)
        sprite.setOriginCenter()
        sprite.setRotation(hitbox.rotation)
        sprite.setScale(hitbox.scale)
      } else
        throw new RuntimeException(
          "cannot update sprite without filling in hitbox information! dir vector for ability probably not set"
        )

    if (getAbility.state.stage == AbilityStage.ChannelStage) {

      val texture =
        channelAnimations(getAbility.state.currentAttackPhase)
          .getKeyFrame(getAbility.state.stageTimer.time, getAbility.channelAnimationLooping)
      updateSprite(texture)

    }

    if (getAbility.state.stage == AbilityStage.ActiveStage) {

      val texture =
        activeAnimations(getAbility.state.currentAttackPhase)
          .getKeyFrame(getAbility.state.stageTimer.time, getAbility.channelAnimationLooping)
      updateSprite(texture)
    }

  }

  def render(drawingLayer: DrawingLayer)(implicit gameState: GameState): Unit =
    if (getAbility.state.stage == AbilityStage.ChannelStage || getAbility.state.stage == AbilityStage.ActiveStage)
      sprite.draw(drawingLayer.spriteBatch)
}
