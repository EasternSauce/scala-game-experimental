package game

import cats.data.State
import com.badlogic.gdx.graphics.g2d.SpriteBatch
import com.badlogic.gdx.graphics.{GL20, OrthographicCamera}
import com.badlogic.gdx.maps.tiled.TiledMap
import com.badlogic.gdx.maps.tiled.renderers.OrthogonalTiledMapRenderer
import com.badlogic.gdx.utils.viewport.{FitViewport, Viewport}
import com.badlogic.gdx.{Gdx, Screen}
import com.softwaremill.quicklens.ModifyPimp
import model.GameState.updateCreatures
import model.{GameState, Player}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object PlayScreen extends Screen {

  var spriteBatch: SpriteBatch = _
  var maps: Map[String, TiledMap] = _

  var worldCamera: OrthographicCamera = _

  var worldViewport: Viewport = _

  val cameraPosX = 5
  val cameraPosY = 9

  var gameState: AtomicSTRef[GameState] = _

  var tiledMapRenderer: OrthogonalTiledMapRenderer = _

  def init(): Unit = {
    worldCamera = new OrthographicCamera()

    worldViewport = new FitViewport(
      Constants.ViewpointWorldWidth / Constants.PPM,
      Constants.ViewpointWorldHeight / Constants.PPM,
      worldCamera
    )

    tiledMapRenderer = new OrthogonalTiledMapRenderer(maps("area1"), Constants.MapScale / Constants.PPM)

    gameState = AtomicSTRef(GameState(creatures = Map("player" -> Player("player", 0, 0))))

    Future {
      while(true) {
        Thread.sleep(500)
        val stateChange: State[GameState, Unit] = {
          State.modify {
            implicit state: GameState => GameState.creatureLens("player").using(_.modify(_.posX).using(_ + 1))
          }
        }
        gameState.commit(stateChange)
      }

    }

    Future {
      while(true) {
        Thread.sleep(1000)
        val stateChange: State[GameState, Unit] = {
          State.modify {
            implicit state: GameState => GameState.creatureLens("player").using(_.modify(_.posY).using(_ + 1))
          }
        }
        gameState.commit(stateChange)
      }

    }
  }

  def setSpriteBatch(spriteBatch: SpriteBatch): Unit = this.spriteBatch = spriteBatch

  def setMaps(maps: Map[String, TiledMap]): Unit = this.maps = maps

  def updateCamera(): Unit = {

    val camPosition = worldCamera.position

    val playerPosX = gameState.aref.get().creatures("player").posX
    val playerPosY = gameState.aref.get().creatures("player").posY

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

    implicit val gs: GameState = gameState.aref.get()

//    gameState = updateCreatures().run(gameState).value._1

//    gameState.commit(updateCreatures())

    println(gameState.aref.get().creatures("player").posX + " " + gameState.aref.get().creatures("player").posY)
  }

  override def resize(width: Int, height: Int): Unit = {
    worldViewport.update(width, height)
  }

  override def pause(): Unit = {}

  override def resume(): Unit = {}

  override def hide(): Unit = {}

  override def dispose(): Unit = {}
}
