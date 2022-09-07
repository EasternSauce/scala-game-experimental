package com.easternsauce.game

import com.badlogic.gdx.graphics.g2d.{SpriteBatch, TextureRegion}
import com.badlogic.gdx.graphics.{Color, Texture}
import com.badlogic.gdx.math.{Matrix4, Rectangle}
import space.earlygrey.shapedrawer.ShapeDrawer

case class DrawingLayer() {
  val spriteBatch: SpriteBatch = new SpriteBatch()
  val shapeDrawer: ShapeDrawer = new ShapeDrawer(spriteBatch, createTextureAndRegion())

  private var texture: Texture = _

  def begin(): Unit = spriteBatch.begin()

  def end(): Unit = spriteBatch.end()

  def filledRectangle(rect: Rectangle, color: Color): Unit = shapeDrawer.filledRectangle(rect, color)

  def setProjectionMatrix(projection: Matrix4): Unit = spriteBatch.setProjectionMatrix(projection)

  private def createTextureAndRegion(): TextureRegion = {
    import com.badlogic.gdx.graphics.Pixmap.Format
    import com.badlogic.gdx.graphics.g2d.TextureRegion
    import com.badlogic.gdx.graphics.{Pixmap, Texture}
    val pixmap = new Pixmap(1, 1, Format.RGBA8888)
    pixmap.setColor(Color.WHITE)
    pixmap.drawPixel(0, 0)
    texture = new Texture(pixmap) //remember to dispose of later

    pixmap.dispose()
    new TextureRegion(texture, 0, 0, 1, 1)
  }

  def dispose(): Unit =
    texture.dispose()
}
