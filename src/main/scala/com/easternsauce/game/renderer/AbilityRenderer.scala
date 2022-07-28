package com.easternsauce.game.renderer

import com.badlogic.gdx.graphics.g2d.{Animation, Sprite, SpriteBatch, TextureAtlas, TextureRegion}
import com.easternsauce.model.GameState
import com.easternsauce.model.GameState.getAbility
import com.easternsauce.model.ability.{AbilityStage, AbilityState}
import com.easternsauce.model.ids.AbilityId

case class AbilityRenderer(abilityId: AbilityId) {
  var sprite: Sprite = _

  var channelAnimation: Animation[TextureRegion] = _
  var activeAnimation: Animation[TextureRegion] = _

  var channelTextureRegion: TextureRegion = _
  var activeTextureRegion: TextureRegion = _

  def init(atlas: TextureAtlas)(implicit gameState: GameState): Unit = {
    sprite = new Sprite()

    val ability = getAbility(abilityId)(gameState)

    channelTextureRegion = atlas.findRegion(ability.channelSpriteType)
    activeTextureRegion = atlas.findRegion(ability.activeSpriteType)

    val channelFrames = for { i <- (0 until ability.channelFrameCount).toArray } yield {
      new TextureRegion(channelTextureRegion, i * ability.textureWidth, 0, ability.textureWidth, ability.textureHeight)
    }
    channelAnimation = new Animation[TextureRegion](ability.channelFrameDuration, channelFrames: _*)

    val activeFrames = for { i <- (0 until ability.activeFrameCount).toArray } yield {
      new TextureRegion(activeTextureRegion, i * ability.textureWidth, 0, ability.textureWidth, ability.textureHeight)
    }
    activeAnimation = new Animation[TextureRegion](ability.activeFrameDuration, activeFrames: _*)
  }

  def update()(implicit gameState: GameState): Unit = {
    val ability = getAbility(abilityId)(gameState)

    def updateSprite(texture: TextureRegion): Unit = {
      val ability = getAbility(abilityId)(gameState)

      if (ability.state.attack.nonEmpty) {
        sprite.setRegion(texture)
        sprite.setSize(ability.state.attack.get.hitbox.width, ability.state.attack.get.hitbox.height)
        sprite.setCenter(ability.state.attack.get.hitbox.pos.x, ability.state.attack.get.hitbox.pos.y)
        sprite.setOriginCenter()
        sprite.setRotation(ability.state.attack.get.hitbox.rotation)
        sprite.setScale(ability.state.attack.get.hitbox.scale)
      }

    }

    if (ability.state.stage == AbilityStage.Channel) {

      val texture =
        channelAnimation.getKeyFrame(ability.state.stageTimer.time, ability.channelAnimationLooping)
      updateSprite(texture)

    }

    if (ability.state.stage == AbilityStage.Active) {

      val texture =
        activeAnimation.getKeyFrame(ability.state.stageTimer.time, ability.channelAnimationLooping)
      updateSprite(texture)
    }

  }

  def render(batch: SpriteBatch)(implicit gameState: GameState): Unit = {
    val ability = getAbility(abilityId)(gameState)

    if (ability.state.stage == AbilityStage.Channel || ability.state.stage == AbilityStage.Active) {
      sprite.draw(batch)
    }
  }
}
