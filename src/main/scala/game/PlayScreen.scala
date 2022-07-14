package game

import com.badlogic.gdx.graphics.g2d.SpriteBatch
import com.badlogic.gdx.graphics.{GL20, OrthographicCamera}
import com.badlogic.gdx.maps.tiled.TiledMap
import com.badlogic.gdx.maps.tiled.renderers.OrthogonalTiledMapRenderer
import com.badlogic.gdx.utils.viewport.{FitViewport, Viewport}
import com.badlogic.gdx.{Gdx, Screen}
import model.GameState.updateCreatures
import model.{GameState, Player}

object PlayScreen extends Screen {

  var spriteBatch: SpriteBatch = _
  var maps: Map[String, TiledMap] = _

  var worldCamera: OrthographicCamera = _

  var worldViewport: Viewport = _

  val cameraPosX = 5
  val cameraPosY = 9

  implicit var gameState: GameState = _

  var tiledMapRenderer: OrthogonalTiledMapRenderer = _

  def init(): Unit = {
    worldCamera = new OrthographicCamera()

    worldViewport = new FitViewport(
      Constants.ViewpointWorldWidth / Constants.PPM,
      Constants.ViewpointWorldHeight / Constants.PPM,
      worldCamera
    )

    tiledMapRenderer = new OrthogonalTiledMapRenderer(maps("area1"), Constants.MapScale / Constants.PPM)

    gameState = GameState(creatures = Map("player" -> Player("player", 0, 0)))
  }

  def setSpriteBatch(spriteBatch: SpriteBatch): Unit = this.spriteBatch = spriteBatch

  def setMaps(maps: Map[String, TiledMap]): Unit = this.maps = maps

  def updateCamera(): Unit = {

    val camPosition = worldCamera.position

    val playerPosX = gameState.creatures("player").posX
    val playerPosY = gameState.creatures("player").posY

    camPosition.x = (math.floor(playerPosX * 100) / 100).toFloat
    camPosition.y = (math.floor(playerPosY * 100) / 100).toFloat

    worldCamera.update()

  }

  override def show(): Unit = {}

  override def render(delta: Float): Unit = {
    update(delta)

    spriteBatch.setProjectionMatrix(worldCamera.combined)

    Gdx.gl.glClearColor(0, 0, 0, 1)

    val coverageBuffer = if (Gdx.graphics.getBufferFormat.coverageSampling) GL20.GL_COVERAGE_BUFFER_BIT_NV else 0
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT | GL20.GL_DEPTH_BUFFER_BIT | coverageBuffer)

    tiledMapRenderer.render(Array(0, 1))

    spriteBatch.begin()

    spriteBatch.end()

    tiledMapRenderer.render(Array(2, 3))
  }

  def update(delta: Float): Unit = {
    tiledMapRenderer.setView(worldCamera)
    updateCamera()

    gameState = updateCreatures().run(gameState).value._1

    println(gameState.creatures("player").posX + " " + gameState.creatures("player").posY)
  }

  override def resize(width: Int, height: Int): Unit = {
    worldViewport.update(width, height)
  }

  override def pause(): Unit = {}

  override def resume(): Unit = {}

  override def hide(): Unit = {}

  override def dispose(): Unit = {}
}
