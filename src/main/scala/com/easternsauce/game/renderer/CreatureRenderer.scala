package com.easternsauce.game.renderer

import com.badlogic.gdx.graphics.g2d._
import com.easternsauce.model.GameState
import com.easternsauce.model.GameState.{getCreature, player}
import com.easternsauce.model.WorldDirection.WorldDirection
import com.easternsauce.model.ids.CreatureId

case class CreatureRenderer(creatureId: CreatureId) {
  implicit val _creatureId: CreatureId = creatureId
  var sprite: Sprite                   = _

  var facingTextures: Array[TextureRegion] = _

  var runningAnimations: Array[Animation[TextureRegion]] = _

  var textureRegion: TextureRegion = _

  def init(atlas: TextureAtlas)(implicit gameState: GameState): Unit = {
    sprite = new Sprite()

    facingTextures = new Array[TextureRegion](4)

    runningAnimations = new Array[Animation[TextureRegion]](4)

    val creature = gameState.creatures(creatureId)

    textureRegion = atlas.findRegion(creature.textureName)

    for (i <- 0 until 4)
      facingTextures(i) = new TextureRegion(
        textureRegion,
        creature.neutralStanceFrame * creature.textureWidth,
        i * creature.textureHeight,
        creature.textureWidth,
        creature.textureHeight
      )

    for (i <- 0 until 4) {
      val frames =
        for { j <- (0 until creature.frameCount).toArray } yield new TextureRegion(
          textureRegion,
          j * creature.textureWidth,
          i * creature.textureHeight,
          creature.textureWidth,
          creature.textureHeight
        )
      runningAnimations(i) = new Animation[TextureRegion](creature.frameDuration, frames: _*)

    }

  }

  def runningAnimation(currentDirection: WorldDirection)(implicit gameState: GameState): TextureRegion = {
    val creature = gameState.creatures(creatureId)

    runningAnimations(creature.dirMap(currentDirection))
      .getKeyFrame(player.state.animationTimer.time, true)
  }

  def facingTexture(
    gameState: GameState,
    currentDirection: WorldDirection
  ): TextureRegion = {
    val creature = gameState.creatures(creatureId)

    facingTextures(creature.dirMap(currentDirection))
  }

  def update()(implicit gameState: GameState): Unit = {

    val texture =
      if (!getCreature.isMoving) facingTexture(gameState, getCreature.facingDirection)
      else runningAnimation(getCreature.facingDirection)
    sprite.setRegion(texture)
    sprite.setCenter(getCreature.state.pos.x, getCreature.state.pos.y)
    sprite.setSize(getCreature.width, getCreature.height)
  }

  def render(batch: SpriteBatch): Unit =
    sprite.draw(batch)
}
