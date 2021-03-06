package com.easternsauce.game.renderer

import com.badlogic.gdx.graphics.g2d.{SpriteBatch, TextureAtlas}
import com.badlogic.gdx.maps.tiled.TiledMap
import com.easternsauce.model.GameState
import com.easternsauce.model.GameState.getCreature
import com.easternsauce.model.ids.{AbilityId, AreaId, CreatureId}

object SpriteRendererController {
  var creatureSpriteRenderers: Map[CreatureId, CreatureRenderer] = _
  var abilitySpriteRenderers: Map[AbilityId, AbilityRenderer] = _
  var atlas: TextureAtlas = _

  def init(atlas: TextureAtlas, maps: Map[AreaId, TiledMap])(implicit gameState: GameState): Unit = {
    this.atlas = atlas

    creatureSpriteRenderers =
      gameState.creatures.keys.map(creatureId => creatureId -> CreatureRenderer(creatureId)).toMap
    creatureSpriteRenderers.values.foreach(_.init(atlas))

    abilitySpriteRenderers = Map()
  }

  def update()(implicit gameState: GameState): Unit = {
    creatureSpriteRenderers.foreach {
      case (_, renderer) => renderer.update()
    }
    abilitySpriteRenderers.foreach {
      case (_, renderer) => renderer.update()
    }
  }

  def addRenderer(abilityId: AbilityId)(implicit gameState: GameState): Unit = {
    abilitySpriteRenderers = abilitySpriteRenderers.updated(abilityId, AbilityRenderer(abilityId))
    abilitySpriteRenderers(abilityId).init(atlas)
  }

  def renderAliveEntities(batch: SpriteBatch, debugEnabled: Boolean)(implicit gameState: GameState): Unit = {
    gameState.creatures.filter { case (_, creature) => creature.isAlive }.keys.foreach { implicit creatureId =>
      if (
        creatureSpriteRenderers.contains(creatureId) &&
        getCreature.state.areaId == gameState.currentAreaId
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
