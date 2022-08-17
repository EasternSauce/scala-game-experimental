package com.easternsauce.game.renderer

import com.badlogic.gdx.graphics.g2d._
import com.easternsauce.model.GameState
import com.easternsauce.model.GameState.getAbility
import com.easternsauce.model.ability.AbilityStage
import com.easternsauce.model.ids.AbilityId

case class AbilityRenderer(abilityId: AbilityId) {
  implicit val _abilityId: AbilityId = abilityId

  var sprite: Sprite = _

  var channelAnimation: Animation[TextureRegion] = _
  var activeAnimation: Animation[TextureRegion]  = _

  var channelTextureRegion: TextureRegion = _
  var activeTextureRegion: TextureRegion  = _

  def init(atlas: TextureAtlas)(implicit gameState: GameState): Unit = {
    sprite = new Sprite()

    channelTextureRegion = atlas.findRegion(getAbility.channelSpriteType)
    activeTextureRegion = atlas.findRegion(getAbility.activeSpriteType)

    val channelFrames =
      for { i <- (0 until getAbility.channelFrameCount).toArray } yield new TextureRegion(
        channelTextureRegion,
        i * getAbility.textureWidth,
        0,
        getAbility.textureWidth,
        getAbility.textureHeight
      )
    channelAnimation = new Animation[TextureRegion](getAbility.channelFrameDuration, channelFrames: _*)

    val activeFrames =
      for { i <- (0 until getAbility.activeFrameCount).toArray } yield new TextureRegion(
        activeTextureRegion,
        i * getAbility.textureWidth,
        0,
        getAbility.textureWidth,
        getAbility.textureHeight
      )
    activeAnimation = new Animation[TextureRegion](getAbility.activeFrameDuration, activeFrames: _*)
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

    if (getAbility.state.stage == AbilityStage.Channel) {

      val texture =
        channelAnimation.getKeyFrame(getAbility.state.stageTimer.time, getAbility.channelAnimationLooping)
      updateSprite(texture)

    }

    if (getAbility.state.stage == AbilityStage.Active) {

      val texture =
        activeAnimation.getKeyFrame(getAbility.state.stageTimer.time, getAbility.channelAnimationLooping)
      updateSprite(texture)
    }

  }

  def render(batch: SpriteBatch)(implicit gameState: GameState): Unit =
    if (getAbility.state.stage == AbilityStage.Channel || getAbility.state.stage == AbilityStage.Active)
      sprite.draw(batch)
}
