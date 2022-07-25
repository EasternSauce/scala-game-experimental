package com.easternsauce.game.renderer

import com.badlogic.gdx.graphics.g2d._
import com.easternsauce.model.GameState
import com.easternsauce.model.GameState.{creature, player}
import com.easternsauce.model.WorldDirection.WorldDirection
import com.easternsauce.model.ids.CreatureId

case class CreatureRenderer(creatureId: CreatureId) {
  var sprite: Sprite = _

  var facingTextures: Array[TextureRegion] = _

  var runningAnimations: Array[Animation[TextureRegion]] = _

  var textureRegion: TextureRegion = _

  def init(gameState: GameState, atlas: TextureAtlas): Unit = {
    sprite = new Sprite()

    facingTextures = new Array[TextureRegion](4)

    runningAnimations = new Array[Animation[TextureRegion]](4)

    val creature = gameState.creatures(creatureId)

    textureRegion = atlas.findRegion(creature.textureName)

    for (i <- 0 until 4) {
      facingTextures(i) = new TextureRegion(
        textureRegion,
        creature.neutralStanceFrame * creature.textureWidth,
        i * creature.textureHeight,
        creature.textureWidth,
        creature.textureHeight
      )

    }

    for (i <- 0 until 4) {
      val frames = for { j <- (0 until creature.frameCount).toArray } yield {
        new TextureRegion(
          textureRegion,
          j * creature.textureWidth,
          i * creature.textureHeight,
          creature.textureWidth,
          creature.textureHeight
        )
      }
      runningAnimations(i) = new Animation[TextureRegion](creature.frameDuration, frames: _*)

    }

  }

  def runningAnimation(implicit gameState: GameState, currentDirection: WorldDirection): TextureRegion = {
    val creature = gameState.creatures(creatureId)

    runningAnimations(creature.dirMap(currentDirection))
      .getKeyFrame(player.state.animationTimer.time, true)
  }

  def facingTexture(gameState: GameState, currentDirection: WorldDirection): TextureRegion = {
    val creature = gameState.creatures(creatureId)

    facingTextures(creature.dirMap(currentDirection))
  }

  def update(implicit gameState: GameState): Unit = {

    val texture =
      if (!creature(creatureId).isMoving) facingTexture(gameState, creature(creatureId).facingDirection)
      else runningAnimation(gameState, creature(creatureId).facingDirection)
    sprite.setRegion(texture)
    sprite.setCenter(creature(creatureId).state.pos.x, creature(creatureId).state.pos.y)
    sprite.setSize(creature(creatureId).width, creature(creatureId).height)

//    if (creature(creatureId).isAlive && creature(creatureId).isEffectActive("immunityFrames")) {
//      val alpha = creature(creatureId).params.effects("immunityFrames").remainingTime * 35f
//      val colorComponent = 0.3f + 0.7f * (Math.sin(alpha).toFloat + 1f) / 2f
//      sprite.setColor(1f, colorComponent, colorComponent, 1f)
//    } else {
//      sprite.setColor(1, 1, 1, 1)
//    }
//
//    if (!creature(creatureId).isAlive) {
//      sprite.setOriginCenter()
//      sprite.setRotation(90f)
//    }
//
//    abilityComponentRenderers.foreach(_.update(gameState))

  }

  def render(batch: SpriteBatch): Unit = {
    sprite.draw(batch)
  }
}
