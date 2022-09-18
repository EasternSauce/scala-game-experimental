package com.easternsauce.game.renderer

import com.badlogic.gdx.scenes.scene2d.ui.Image
import com.easternsauce.game.physics.AreaGateBody
import com.easternsauce.game.{Assets, DrawingLayer}
import com.easternsauce.model.GameState

case class AreaGateRenderer(areaGate: AreaGateBody) { // TODO: should we read it from game state instead of passing body reference?
  private val downArrowImageFrom = new Image(Assets.atlas.findRegion("downarrow"))
  private val downArrowImageTo = new Image(Assets.atlas.findRegion("downarrow"))

  downArrowImageFrom.setPosition(areaGate.x1 - areaGate.width / 2f, areaGate.y1 - areaGate.height / 2f)
  downArrowImageTo.setPosition(areaGate.x2 - areaGate.width / 2f, areaGate.y2 - areaGate.height / 2f)
  downArrowImageFrom.setWidth(areaGate.width)
  downArrowImageFrom.setHeight(areaGate.height)
  downArrowImageTo.setWidth(areaGate.width)
  downArrowImageTo.setHeight(areaGate.height)

  def render(gameState: GameState, drawingLayer: DrawingLayer): Unit = {
    val areaId = gameState.currentAreaId

    if (areaId == areaGate.area1Id) downArrowImageFrom.draw(drawingLayer.spriteBatch, 1.0f)
    if (areaId == areaGate.area2Id) downArrowImageTo.draw(drawingLayer.spriteBatch, 1.0f)
  }

}
