package com.easternsauce.game.renderer

import com.badlogic.gdx.graphics.Color
import com.badlogic.gdx.graphics.g2d.TextureAtlas
import com.badlogic.gdx.maps.tiled.TiledMap
import com.badlogic.gdx.math.Rectangle
import com.easternsauce.game.DrawingLayer
import com.easternsauce.game.physics.AreaGateBody
import com.easternsauce.model.GameState.getCreature
import com.easternsauce.model.ids.{AbilityId, AreaId, CreatureId}
import com.easternsauce.model.{GameState, Vec2}

object RendererController {
  var creatureSpriteRenderers: Map[CreatureId, CreatureRenderer] = _
  var abilitySpriteRenderers: Map[AbilityId, AbilityRenderer] = _
  var areaGateRenderers: List[AreaGateRenderer] = _

  var atlas: TextureAtlas = _

  def init(atlas: TextureAtlas, maps: Map[AreaId, TiledMap], areaGates: List[AreaGateBody])(implicit
    gameState: GameState
  ): Unit = {
    this.atlas = atlas

    creatureSpriteRenderers =
      gameState.creatures.keys.map(creatureId => creatureId -> CreatureRenderer(creatureId)).toMap
    creatureSpriteRenderers.values.foreach(_.init(atlas))

    abilitySpriteRenderers = Map()

    areaGateRenderers = areaGates.map(AreaGateRenderer)

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

  def renderAliveCreatures(drawingLayer: DrawingLayer, debugEnabled: Boolean)(implicit gameState: GameState): Unit = {

    gameState.creatures.filter { case (_, creature) => creature.isAlive }.keys.foreach { implicit creatureId =>
      if (
        creatureSpriteRenderers.contains(creatureId) &&
        getCreature.state.areaId == gameState.currentAreaId
      )
        creatureSpriteRenderers(creatureId).render(drawingLayer)
    }

    gameState.creatures.filter { case (_, creature) => creature.isAlive }.keys.foreach { creatureId =>
      if (
        creatureSpriteRenderers.contains(creatureId) &&
        gameState.creatures(creatureId).state.areaId == gameState.currentAreaId
      )
        creatureSpriteRenderers(creatureId).renderLifeBar(drawingLayer, gameState)
    }

  }

  def renderDeadCreatures(batch: DrawingLayer, debugEnabled: Boolean)(implicit gameState: GameState): Unit =
    gameState.creatures.filter { case (_, creature) => !creature.isAlive }.keys.foreach { creatureId =>
      if (
        creatureSpriteRenderers.contains(creatureId) &&
        gameState.creatures(creatureId).state.areaId == gameState.currentAreaId
      )
        creatureSpriteRenderers(creatureId).render(batch)
    }

  def renderAbilities(drawingLayer: DrawingLayer)(implicit gameState: GameState): Unit =
    gameState.abilities.keys.foreach { abilityId =>
      if (abilitySpriteRenderers.contains(abilityId))
        abilitySpriteRenderers(abilityId).render(drawingLayer)
    }

  def renderLifeAndStamina(drawingLayer: DrawingLayer)(implicit gameState: GameState): Unit = {
    val player = GameState.player

    val maxLifeRect = new Rectangle(10, 40, 100, 10)
    val lifeRect =
      new Rectangle(10, 40, 100 * player.state.life / player.state.maxLife, 10)
    val maxStaminaRect = new Rectangle(10, 25, 100, 10)
    val staminaRect =
      new Rectangle(10, 25, 100 * player.state.stamina / player.state.maxStamina, 10)

    drawingLayer.shapeDrawer.filledRectangle(maxLifeRect, Color.ORANGE)

    if (player.state.life <= player.state.maxLife) {
      drawingLayer.shapeDrawer.filledRectangle(lifeRect, Color.RED)
    } else {
      drawingLayer.shapeDrawer.filledRectangle(maxLifeRect, Color.ROYAL)
    }

    drawingLayer.shapeDrawer.filledRectangle(maxStaminaRect, Color.ORANGE)
    drawingLayer.shapeDrawer.filledRectangle(staminaRect, Color.GREEN)
  }

  def renderHud(drawingLayer: DrawingLayer, mousePosition: Vec2)(implicit gameState: GameState): Unit = {

//    inventoryRenderer.render(gameState, batch, mousePosition)
//
//    lootPickupMenuRenderer.render(gameState, batch, mousePosition)

    renderLifeAndStamina(drawingLayer)
  }

  def renderAreaGates(gameState: GameState, drawingLayer: DrawingLayer): Unit = {
    areaGateRenderers.foreach(_.render(gameState, drawingLayer))
  }
}
