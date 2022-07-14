package game

import com.badlogic.gdx.graphics.g2d.SpriteBatch
import com.badlogic.gdx.{Gdx, Screen}
import com.badlogic.gdx.graphics.{GL20, OrthographicCamera}
import com.badlogic.gdx.maps.tiled.TiledMap
import com.badlogic.gdx.maps.tiled.renderers.OrthogonalTiledMapRenderer
import com.badlogic.gdx.utils.viewport.{FitViewport, Viewport}

class PlayScreen(
                  val spriteBatch: SpriteBatch,
                  val maps: Map[String, TiledMap]
                ) extends Screen {
  val worldCamera: OrthographicCamera = new OrthographicCamera()

  val worldViewport: Viewport =
    new FitViewport(
      1650f / 32,
      864f / 32,
      worldCamera
    )

  val cameraPosX = 5
  val cameraPosY = 9

  private var tiledMapRenderer: OrthogonalTiledMapRenderer = new OrthogonalTiledMapRenderer(maps("area1"), 4.0f / 32)

  def updateCamera(): Unit = {

    val camPosition = worldCamera.position

    camPosition.x = (math.floor(cameraPosX * 100) / 100).toFloat
    camPosition.y = (math.floor(cameraPosY * 100) / 100).toFloat

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



    println("zzz")

    spriteBatch.begin()

    spriteBatch.end()

    tiledMapRenderer.render(Array(2, 3))
  }


  def update(delta: Float): Unit = {
    tiledMapRenderer.setView(worldCamera)
    updateCamera()
  }

    override def resize(width: Int, height: Int): Unit = {
      worldViewport.update(width, height)
    }

  override def pause(): Unit = {}

  override def resume(): Unit = {}

  override def hide(): Unit = {}

  override def dispose(): Unit = {}
}
