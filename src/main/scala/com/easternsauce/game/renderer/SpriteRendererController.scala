package com.easternsauce.game.renderer

import com.badlogic.gdx.graphics.g2d.{SpriteBatch, TextureAtlas}
import com.badlogic.gdx.maps.tiled.TiledMap
import com.easternsauce.model.GameState
import com.easternsauce.model.GameState.getCreature
import com.easternsauce.model.ids.{AbilityId, AreaId, CreatureId}

object SpriteRendererController {
  var creatureSpriteRenderers: Map[CreatureId, CreatureRenderer] = _
  var abilitySpriteRenderers: Map[AbilityId, AbilityRenderer] = _

  def init(atlas: TextureAtlas, maps: Map[AreaId, TiledMap])(implicit gameState: GameState): Unit = {
    creatureSpriteRenderers =
      gameState.creatures.keys.map(creatureId => creatureId -> CreatureRenderer(creatureId)).toMap
    creatureSpriteRenderers.values.foreach(_.init(atlas))

    abilitySpriteRenderers =
      gameState.abilities.keys.map(abilityId => abilityId -> AbilityRenderer(abilityId)).toMap
    abilitySpriteRenderers.values.foreach(_.init(atlas))
  }

  def update()(implicit gameState: GameState): Unit = {
    gameState.creatures.keys.foreach { creatureId =>
      creatureSpriteRenderers(creatureId).update()
    }
    gameState.abilities.keys.foreach { abilityId =>
      abilitySpriteRenderers(abilityId).update()
    }
  }

  def renderAliveEntities(batch: SpriteBatch, debugEnabled: Boolean)(implicit gameState: GameState): Unit = {
    gameState.creatures.filter { case (_, creature) => creature.isAlive }.keys.foreach { creatureId =>
      if (
        creatureSpriteRenderers.contains(creatureId) &&
        getCreature(creatureId).state.areaId == gameState.currentAreaId
      ) {
        creatureSpriteRenderers(creatureId).render(batch)
      }
    }
  }

  def renderAbilities(batch: SpriteBatch)(implicit gameState: GameState): Unit = {
    gameState.abilities.keys.foreach { abilityId =>
      if (abilitySpriteRenderers.contains(abilityId)) {
        abilitySpriteRenderers(abilityId).render(batch)
      }
    }
  }
}
