package com.easternsauce.game.renderer

import com.badlogic.gdx.graphics.g2d.{SpriteBatch, TextureAtlas}
import com.badlogic.gdx.maps.tiled.TiledMap
import com.easternsauce.model.GameState
import com.easternsauce.model.GameState.creature
import com.easternsauce.model.ids.{AreaId, CreatureId}

object SpriteRendererController {
  var creatureSpriteRenderers: Map[CreatureId, CreatureRenderer] = _

  def init(atlas: TextureAtlas, gameState: GameState, maps: Map[AreaId, TiledMap]): Unit = {
    creatureSpriteRenderers =
      gameState.creatures.keys.map(creatureId => creatureId -> CreatureRenderer(creatureId)).toMap
    creatureSpriteRenderers.values.foreach(_.init(gameState, atlas))
  }

  def update(gameState: GameState): Unit = {
    gameState.creatures.keys.foreach { creatureId =>
      creatureSpriteRenderers(creatureId).update(gameState)
    }
  }

  def renderAliveEntities(implicit gameState: GameState, batch: SpriteBatch, debugEnabled: Boolean): Unit = {
    gameState.creatures.filter { case (_, creature) => creature.isAlive }.keys.foreach { creatureId =>
      if (
        creatureSpriteRenderers.contains(creatureId) &&
        creature(creatureId).state.areaId == gameState.currentAreaId
      ) {
        creatureSpriteRenderers(creatureId).render(batch)
      }
    }
  }
}
