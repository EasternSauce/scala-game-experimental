package game

import cats.implicits.toTraverseOps
import com.badlogic.gdx.graphics.g2d.{SpriteBatch, TextureAtlas}
import com.badlogic.gdx.graphics.{GL20, OrthographicCamera}
import com.badlogic.gdx.maps.tiled.TiledMap
import com.badlogic.gdx.maps.tiled.renderers.OrthogonalTiledMapRenderer
import com.badlogic.gdx.physics.box2d.Box2DDebugRenderer
import com.badlogic.gdx.utils.viewport.{FitViewport, Viewport}
import com.badlogic.gdx.{Gdx, Input, Screen}
import game.physics.PhysicsEngineController
import game.renderer.SpriteRendererController
import model.GameState.{creature, handlePlayerMovementInput, updateCreatures}
import model.WorldDirection.WorldDirection
import model._

object PlayScreen extends Screen {

  var spriteBatch: SpriteBatch = _
  var maps: Map[String, TiledMap] = _

  var worldCamera: OrthographicCamera = _

  var worldViewport: Viewport = _

  var gameState: AtomicSTRef[GameState] = _

  var tiledMapRenderer: OrthogonalTiledMapRenderer = _

  val debugEnabled = true

  val b2DebugRenderer: Box2DDebugRenderer = new Box2DDebugRenderer()

  def init(atlas: TextureAtlas): Unit = {
    worldCamera = new OrthographicCamera()

    worldViewport = new FitViewport(
      Constants.ViewpointWorldWidth / Constants.PPM,
      Constants.ViewpointWorldHeight / Constants.PPM,
      worldCamera
    )

    gameState = AtomicSTRef(
      GameState(
        creatures = Map("player" -> Player(CreatureState(id = "player", pos = Vec2(22, 4), areaId = "area1"))),
        currentPlayerId = "player",
        currentAreaId = "area1"
      )
    )

    tiledMapRenderer =
      new OrthogonalTiledMapRenderer(maps(gameState.aref.get().currentAreaId), Constants.MapScale / Constants.PPM)

    SpriteRendererController.init(atlas, gameState.aref.get(), maps)
    PhysicsEngineController.init(gameState = gameState.aref.get(), maps)

  }

  def setSpriteBatch(spriteBatch: SpriteBatch): Unit = this.spriteBatch = spriteBatch

  def setMaps(maps: Map[String, TiledMap]): Unit = this.maps = maps

  def updateCamera(): Unit = {

    val camPosition = worldCamera.position

    implicit val gs: GameState = gameState.aref.get()

    val playerPosX = creature("player").state.pos.x
    val playerPosY = creature("player").state.pos.y

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

    SpriteRendererController.renderAliveEntities(gameState.aref.get(), spriteBatch, debugEnabled)

    spriteBatch.end()

    tiledMapRenderer.render(Array(2, 3))

    val currentTerrain = PhysicsEngineController.physicsWorlds(gameState.aref.get().currentAreaId)

    if (debugEnabled) b2DebugRenderer.render(currentTerrain.b2world, worldCamera.combined)

  }

  def update(delta: Float): Unit = {
    tiledMapRenderer.setView(worldCamera)
    updateCamera()

    SpriteRendererController.update(gameState.aref.get())
    PhysicsEngineController.update(gameState.aref.get())

    updateState(delta)

  }

  def updateState(delta: Float): Unit = {

    implicit val gs: GameState = gameState.aref.get()

    val playerDirectionInput: Map[WorldDirection, Boolean] = {
      Map(
        WorldDirection.Left -> Gdx.input.isKeyPressed(Input.Keys.A),
        WorldDirection.Right -> Gdx.input.isKeyPressed(Input.Keys.D),
        WorldDirection.Down -> Gdx.input.isKeyPressed(Input.Keys.S),
        WorldDirection.Up -> Gdx.input.isKeyPressed(Input.Keys.W)
      )
    }

    gameState.commit(
      List(updateCreatures(delta), handlePlayerMovementInput(playerDirectionInput)).sequence.map(_.flatten)
    )
  }

  override def resize(width: Int, height: Int): Unit = {
    worldViewport.update(width, height)
  }

  override def pause(): Unit = {}

  override def resume(): Unit = {}

  override def hide(): Unit = {}

  override def dispose(): Unit = {}
}
