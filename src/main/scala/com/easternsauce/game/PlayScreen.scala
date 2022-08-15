package com.easternsauce.game

import cats.Monoid
import cats.data.State
import cats.implicits.{catsSyntaxSemigroup, toFoldableOps}
import com.badlogic.gdx.graphics.g2d.{SpriteBatch, TextureAtlas}
import com.badlogic.gdx.graphics.{Color, GL20, OrthographicCamera}
import com.badlogic.gdx.maps.tiled.TiledMap
import com.badlogic.gdx.maps.tiled.renderers.OrthogonalTiledMapRenderer
import com.badlogic.gdx.physics.box2d.Box2DDebugRenderer
import com.badlogic.gdx.utils.viewport.{FitViewport, Viewport}
import com.badlogic.gdx.{Gdx, Input, Screen}
import com.easternsauce.game.physics.{Astar, PhysicsEngineController}
import com.easternsauce.game.renderer.SpriteRendererController
import com.easternsauce.model.GameState._
import com.easternsauce.model.WorldDirection.WorldDirection
import com.easternsauce.model._
import com.easternsauce.model.creature.{Player, Skeleton}
import com.easternsauce.model.ids.{AreaId, CreatureId}
import com.softwaremill.quicklens.ModifyPimp

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
        currentAreaId = AreaId("area1"),
        currentAreaInitialized = false
      )
    )

    tiledMapRenderer =
      new OrthogonalTiledMapRenderer(maps(gameState.aref.get().currentAreaId), Constants.MapScale / Constants.PPM)

    implicit val gs: GameState = gameState.aref.get()

    SpriteRendererController.init(atlas, maps)
    PhysicsEngineController.init(maps)

  }

  def setSpriteBatch(spriteBatch: SpriteBatch): Unit = this.spriteBatch = spriteBatch
  def setHudBatch(hudBatch: SpriteBatch): Unit = this.hudBatch = hudBatch

  def setMaps(maps: Map[AreaId, TiledMap]): Unit = this.maps = maps

  def updateCamera(): Unit = {

    val camPosition = worldCamera.position

    implicit val gs: GameState = gameState.aref.get()

    implicit val playerId: CreatureId = CreatureId("player")

    val playerPosX = getCreature.state.pos.x
    val playerPosY = getCreature.state.pos.y

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

    SpriteRendererController.renderAliveEntities(spriteBatch, debugEnabled)(gameState.aref.get())

    spriteBatch.end()

    hudBatch.begin()

    import com.easternsauce.game.Assets.bitmapFontToEnrichedBitmapFont
    val fps = Gdx.graphics.getFramesPerSecond
    Assets.defaultFont.draw(hudBatch, s"$fps fps", 3, Constants.WindowHeight - 3, Color.WHITE)

    hudBatch.end()

    tiledMapRenderer.render(Array(2, 3))

    spriteBatch.begin()

    SpriteRendererController.renderAbilities(spriteBatch)(gameState.aref.get())

    spriteBatch.end()

    val currentTerrain = PhysicsEngineController.physicsWorlds(gameState.aref.get().currentAreaId)

    if (debugEnabled) b2DebugRenderer.render(currentTerrain.b2world, worldCamera.combined)

  }

  def processExternalEvents()(implicit gameState: GameState, events: List[ExternalEvent]): Unit = {
    events.foreach {
      case AbilityBodyCreateEvent(abilityId)           => PhysicsEngineController.addAbilityBody(abilityId)
      case AbilityBodyActivateEvent(abilityId)         => PhysicsEngineController.activateAbilityBody(abilityId)
      case AbilitySpriteRendererCreateEvent(abilityId) => SpriteRendererController.addRenderer(abilityId)
      case AbilityBodyDestroyEvent(abilityId)          => PhysicsEngineController.destroyAbilityBody(abilityId)
    }
  }

  def update(delta: Float): Unit = {
    tiledMapRenderer.setView(worldCamera)
    updateCamera()

    // single frame calculation + side effects
    implicit val (gameState, events) = commitUpdatedState(delta)

    processExternalEvents()

    SpriteRendererController.update()
    PhysicsEngineController.update()

  }

  def commitUpdatedState(delta: Float): (GameState, List[ExternalEvent]) = {

    val playerDirectionInput: Map[WorldDirection, Boolean] = {
      Map(
        WorldDirection.Left -> Gdx.input.isKeyPressed(Input.Keys.A),
        WorldDirection.Right -> Gdx.input.isKeyPressed(Input.Keys.D),
        WorldDirection.Down -> Gdx.input.isKeyPressed(Input.Keys.S),
        WorldDirection.Up -> Gdx.input.isKeyPressed(Input.Keys.W)
      )
    }
    implicit val gs: GameState = gameState.aref.get()

    val singularGameFrame = if (!gs.currentAreaInitialized) {
      initializeArea(gs.currentAreaId)
    } else {
      updateCreaturePhysics() |+|
        updateCreatures(delta) |+|
        updateAbilities(delta) |+|
        updateAreas() |+|
        handlePlayerMovementInput(playerDirectionInput)
    }

    gameState.commit(singularGameFrame)
  }

  private def updateAreas()(implicit gameState: GameState): GameStateTransition = {
    Monoid[GameStateTransition].empty
  }

  private def updateAbilities(delta: Float)(implicit gameState: GameState): GameStateTransition = {
    gameState.abilities.keys.toList.foldMap(implicit id => getAbility.update(delta))
  }

  // forcefully set creature positions to physics engine values
  private def updateCreaturePhysics()(implicit gameState: GameState): GameStateTransition = {
    gameState.creatures.keys.toList.foldMap(handleCreaturePhysicsUpdate)
  }

  private def updateCreatures(delta: Float)(implicit gameState: GameState): GameStateTransition = {
    gameState.creatures.keys.toList.foldMap(implicit id => getCreature.update(delta) |+| processCreaturePathfinding())
  }

  private def processCreaturePathfinding()(implicit
    creatureId: CreatureId,
    gameState: GameState
  ): GameStateTransition = {
    if (
      getCreature.state.areaId == gameState.currentAreaId &&
      getCreature.isEnemy &&
      getCreature.state.targetCreatureId.nonEmpty &&
      (getCreature.state.forcePathCalculation || getCreature.state.pathCalculationCooldownTimer.time > 1f)
    ) {
      val target = gameState.creatures(getCreature.state.targetCreatureId.get)
      val terrain = PhysicsEngineController.physicsWorlds(getCreature.state.areaId)

      val isLineOfSight = terrain.isLineOfSight(getCreature.state.pos, target.state.pos)

      if (!isLineOfSight) {
        val path = Astar.findPath(terrain, getCreature.state.pos, target.state.pos, getCreature.capability)

        State[GameState, List[ExternalEvent]] { implicit gameState =>
          (
            modifyCreature(
              _.modify(_.state.pathTowardsTarget)
                .setTo(Some(path))
                .modify(_.state.pathCalculationCooldownTimer)
                .using(_.restart())
                .modify(_.state.forcePathCalculation)
                .setTo(false)
            ),
            List()
          )
        }

      } else {
        State[GameState, List[ExternalEvent]] { implicit gameState =>
          (modifyCreature(_.modify(_.state.pathTowardsTarget).setTo(None)), List())
        }
      }
    } else {
      Monoid[GameStateTransition].empty
    }
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
