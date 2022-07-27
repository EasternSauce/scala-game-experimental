package com.easternsauce.game

import cats.implicits.{catsSyntaxSemigroup, toFoldableOps}
import com.badlogic.gdx.graphics.g2d.{SpriteBatch, TextureAtlas}
import com.badlogic.gdx.graphics.{Color, GL20, OrthographicCamera}
import com.badlogic.gdx.maps.tiled.TiledMap
import com.badlogic.gdx.maps.tiled.renderers.OrthogonalTiledMapRenderer
import com.badlogic.gdx.physics.box2d.Box2DDebugRenderer
import com.badlogic.gdx.utils.viewport.{FitViewport, Viewport}
import com.badlogic.gdx.{Gdx, Input, Screen}
import com.easternsauce.game.physics.PhysicsEngineController
import com.easternsauce.game.renderer.SpriteRendererController
import com.easternsauce.model.GameState.{creature, handleCreaturePhysicsUpdate, handlePlayerMovementInput, performAction}
import com.easternsauce.model.WorldDirection.WorldDirection
import com.easternsauce.model._
import com.easternsauce.model.creature.{Player, Skeleton}
import com.easternsauce.model.ids.{AreaId, CreatureId}

object PlayScreen extends Screen {

  var spriteBatch: SpriteBatch = _
  var hudBatch: SpriteBatch = _

  var maps: Map[AreaId, TiledMap] = _

  var worldCamera: OrthographicCamera = _
  var hudCamera: OrthographicCamera = _

  var worldViewport: Viewport = _
  var hudViewport: Viewport = _

  var gameState: AtomicSTRef[GameState] = _

  var tiledMapRenderer: OrthogonalTiledMapRenderer = _

  val debugEnabled = true

  val b2DebugRenderer: Box2DDebugRenderer = new Box2DDebugRenderer()

  def init(atlas: TextureAtlas): Unit = {
    worldCamera = new OrthographicCamera()
    hudCamera = {
      val cam = new OrthographicCamera()
      cam.position.set(Constants.WindowWidth / 2f, Constants.WindowHeight / 2f, 0)
      cam
    }

    worldViewport = new FitViewport(
      Constants.ViewpointWorldWidth / Constants.PPM,
      Constants.ViewpointWorldHeight / Constants.PPM,
      worldCamera
    )

    hudViewport = new FitViewport(Constants.WindowWidth.toFloat, Constants.WindowHeight.toFloat, hudCamera)

    gameState = AtomicSTRef(
      GameState(
        creatures = Map(
          CreatureId("player") -> Player(id = CreatureId("player"), areaId = AreaId("area1"), pos = Vec2(22, 4)),
          CreatureId("skellie") -> Skeleton(id = CreatureId("skellie"), areaId = AreaId("area1"), pos = Vec2(24, 4))
        ),
        currentPlayerId = CreatureId("player"),
        currentAreaId = AreaId("area1")
      )
    )

    // init creatures
    gameState.commit(
      gameState.aref.get().creatures.keys.toList.foldMap(creatureId => performAction(CreatureInitAction(creatureId)))
    )

    tiledMapRenderer =
      new OrthogonalTiledMapRenderer(maps(gameState.aref.get().currentAreaId), Constants.MapScale / Constants.PPM)

    SpriteRendererController.init(atlas, gameState.aref.get(), maps)
    PhysicsEngineController.init(gameState = gameState.aref.get(), maps)

  }

  def setSpriteBatch(spriteBatch: SpriteBatch): Unit = this.spriteBatch = spriteBatch
  def setHudBatch(hudBatch: SpriteBatch): Unit = this.hudBatch = hudBatch

  def setMaps(maps: Map[AreaId, TiledMap]): Unit = this.maps = maps

  def updateCamera(): Unit = {

    val camPosition = worldCamera.position

    implicit val gs: GameState = gameState.aref.get()

    val playerPosX = creature(CreatureId("player")).state.pos.x
    val playerPosY = creature(CreatureId("player")).state.pos.y

    camPosition.x = (math.floor(playerPosX * 100) / 100).toFloat
    camPosition.y = (math.floor(playerPosY * 100) / 100).toFloat

    worldCamera.update()

  }

  override def show(): Unit = {}

  override def render(delta: Float): Unit = {
    update(delta)

    spriteBatch.setProjectionMatrix(worldCamera.combined)
    hudBatch.setProjectionMatrix(hudCamera.combined)

    Gdx.gl.glClearColor(0, 0, 0, 1)

    val coverageBuffer = if (Gdx.graphics.getBufferFormat.coverageSampling) GL20.GL_COVERAGE_BUFFER_BIT_NV else 0
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT | GL20.GL_DEPTH_BUFFER_BIT | coverageBuffer)

    tiledMapRenderer.render(Array(0, 1))

    spriteBatch.begin()

    SpriteRendererController.renderAliveEntities(gameState.aref.get(), spriteBatch, debugEnabled)

    spriteBatch.end()

    hudBatch.begin()

    import com.easternsauce.game.Assets.bitmapFontToEnrichedBitmapFont
    val fps = Gdx.graphics.getFramesPerSecond
    Assets.defaultFont.draw(hudBatch, s"$fps fps", 3, Constants.WindowHeight - 3, Color.WHITE)

    hudBatch.end()

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
      gs.creatures.keys.toList.foldMap(handleCreaturePhysicsUpdate) |+|
        gs.creatures.values.toList.foldMap(_.update(delta)) |+|
        gs.abilities.keys.toList.foldMap(id => performAction(AbilityUpdateAction(id, delta))) |+|
        handlePlayerMovementInput(playerDirectionInput)
    )
  }

  override def resize(width: Int, height: Int): Unit = {
    worldViewport.update(width, height)
    hudViewport.update(width, height)
  }

  override def pause(): Unit = {}

  override def resume(): Unit = {}

  override def hide(): Unit = {}

  override def dispose(): Unit = {}
}
