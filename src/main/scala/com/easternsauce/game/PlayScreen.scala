package com.easternsauce.game

import cats.Monoid
import cats.data.State
import cats.implicits.{catsSyntaxSemigroup, toFoldableOps}
import com.badlogic.gdx.graphics.g2d.TextureAtlas
import com.badlogic.gdx.graphics.{Color, GL20, OrthographicCamera}
import com.badlogic.gdx.maps.tiled.TiledMap
import com.badlogic.gdx.maps.tiled.renderers.OrthogonalTiledMapRenderer
import com.badlogic.gdx.math.Vector3
import com.badlogic.gdx.physics.box2d.Box2DDebugRenderer
import com.badlogic.gdx.utils.viewport.{FitViewport, Viewport}
import com.badlogic.gdx.{Gdx, Input, Screen}
import com.easternsauce.game.physics.{AbilityCollisionEvent, Astar, PhysicsEngineController, PhysicsEvent}
import com.easternsauce.game.renderer.RendererController
import com.easternsauce.model.GameState._
import com.easternsauce.model.WorldDirection.WorldDirection
import com.easternsauce.model._
import com.easternsauce.model.creature.{Player, Skeleton}
import com.easternsauce.model.ids.{AreaId, CreatureId}
import com.softwaremill.quicklens.ModifyPimp

object PlayScreen extends Screen {

  var worldDrawingLayer: DrawingLayer = _
  var hudDrawingLayer: DrawingLayer = _

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

    tiledMapRenderer =
      new OrthogonalTiledMapRenderer(maps(gameState.aref.get().currentAreaId), Constants.MapScale / Constants.PPM)

    implicit val gs: GameState = gameState.aref.get()

    RendererController.init(atlas, maps)
    PhysicsEngineController.init(maps)

  }

  def setWorldDrawingLayer(worldDrawingLayer: DrawingLayer): Unit = this.worldDrawingLayer = worldDrawingLayer
  def setHudDrawingLayer(hudDrawingLayer: DrawingLayer): Unit = this.hudDrawingLayer = hudDrawingLayer

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
    implicit val gs: GameState = gameState.aref.get()

    update(delta)

    worldDrawingLayer.setProjectionMatrix(worldCamera.combined)
    hudDrawingLayer.setProjectionMatrix(hudCamera.combined)

    Gdx.gl.glClearColor(0, 0, 0, 1)

    val coverageBuffer =
      if (Gdx.graphics.getBufferFormat.coverageSampling) GL20.GL_COVERAGE_BUFFER_BIT_NV else 0
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT | GL20.GL_DEPTH_BUFFER_BIT | coverageBuffer)

    tiledMapRenderer.render(Array(0, 1))

    worldDrawingLayer.begin()

    RendererController.renderDeadCreatures(worldDrawingLayer, debugEnabled)
    RendererController.renderAliveCreatures(worldDrawingLayer, debugEnabled)

    worldDrawingLayer.end()

    hudDrawingLayer.begin()

    RendererController.renderHud(hudDrawingLayer, mousePosWindowScaled)

    import com.easternsauce.game.Assets.bitmapFontToEnrichedBitmapFont
    val fps = Gdx.graphics.getFramesPerSecond
    Assets.defaultFont.draw(hudDrawingLayer.spriteBatch, s"$fps fps", 3, Constants.WindowHeight - 3, Color.WHITE)

    hudDrawingLayer.end()

    tiledMapRenderer.render(Array(2, 3))

    worldDrawingLayer.begin()

    RendererController.renderAbilities(worldDrawingLayer)

    worldDrawingLayer.end()

    val currentTerrain = PhysicsEngineController.physicsWorlds(gs.currentAreaId)

    if (debugEnabled) b2DebugRenderer.render(currentTerrain.b2world, worldCamera.combined)

  }

  def processPhysicsEvents(physicsEventQueue: List[PhysicsEvent])(implicit gameState: GameState): GameStateTransition =
    physicsEventQueue.foldMap {
      case AbilityCollisionEvent(creatureId, abilityId, collidedCreatureId) =>
        if (gameState.creatures.contains(creatureId)) {
          val ability = gameState.abilities(abilityId)

          val attackingDisallowed: Boolean =
            getCreature(creatureId, gameState).isControlledAutomatically && getCreature(
              collidedCreatureId,
              gameState
            ).isControlledAutomatically

          if (
            getCreature(collidedCreatureId, gameState).isAlive && !attackingDisallowed && !getCreature(
              collidedCreatureId,
              gameState
            ).isEffectActive("immunityFrames")
          ) {
            val attackedCreature = getCreature(collidedCreatureId, gameState)
            val attackingCreature = getCreature(creatureId, gameState)
            val damage =
              /*if (ability.isWeaponAttack) creatures(creatureId).weaponDamage else abilityComponent.damage*/ 20f // TODO
            attackedCreature.takeLifeDamage(
              damage,
              attackingCreature.state.pos.x,
              attackingCreature.state.pos.y,
              ability.knockbackVelocity
            ) |+|
              attackedCreature.activateEffect("immunityFrames", 0.35f) |+|
              attackedCreature.activateEffect("stagger", 0.35f)
          } else Monoid[GameStateTransition].empty

        } else Monoid[GameStateTransition].empty

      case _ => Monoid[GameStateTransition].empty
    }

  def processExternalEvents(events: List[ExternalEvent])(implicit gameState: GameState): Unit =
    events.foreach {
      case AbilityBodyCreateEvent(abilityId) => PhysicsEngineController.addAbilityBody(abilityId)
      case AbilityBodyDestroyEvent(abilityId) =>
        PhysicsEngineController.removeAbilityBody(abilityId)
      case AbilityBodyActivateEvent(abilityId) =>
        PhysicsEngineController.activateAbilityBody(abilityId)
      case AbilitySpriteRendererCreateEvent(abilityId) =>
        RendererController.addRenderer(abilityId)
      case CreatureBodySetSensorEvent(creatureId) =>
        PhysicsEngineController.setCreatureBodyToSensor(creatureId)
    }

  def update(delta: Float): Unit = {
    tiledMapRenderer.setView(worldCamera)
    updateCamera()

    // single frame calculation + side effects
    implicit val (gameState, events) = commitUpdatedState(delta)

    PhysicsEngineController.physicsEventQueue.clear()

    processExternalEvents(events)

    RendererController.update()
    PhysicsEngineController.update()

  }

  def commitUpdatedState(delta: Float): (GameState, List[ExternalEvent]) = {

    val playerDirectionInput: Map[WorldDirection, Boolean] =
      Map(
        WorldDirection.Left -> Gdx.input.isKeyPressed(Input.Keys.A),
        WorldDirection.Right -> Gdx.input.isKeyPressed(Input.Keys.D),
        WorldDirection.Down -> Gdx.input.isKeyPressed(Input.Keys.S),
        WorldDirection.Up -> Gdx.input.isKeyPressed(Input.Keys.W)
      )
    implicit val gs: GameState = gameState.aref.get()

    val singularGameFrame =
      updateCreaturePhysics() |+|
        updateCreatures(delta) |+|
        updateAbilities(delta) |+|
        updateAreas() |+|
        handlePlayerMovementInput(playerDirectionInput) |+|
        processPhysicsEvents(PhysicsEngineController.physicsEventQueue.toList)

    // TODO: add processing physics events above

    gameState.commit(singularGameFrame)
  }

  private def updateAreas()(implicit gameState: GameState): GameStateTransition =
    if (gameState.currentAreaInitialized)
      Monoid[GameStateTransition].empty
    else
      initializeArea(gameState.currentAreaId)

  private def updateAbilities(delta: Float)(implicit gameState: GameState): GameStateTransition =
    if (gameState.currentAreaInitialized)
      gameState.abilities.keys.toList.foldMap(implicit id => getAbility.update(delta))
    else
      Monoid[GameStateTransition].empty

  // forcefully set creature positions to physics engine values
  private def updateCreaturePhysics()(implicit gameState: GameState): GameStateTransition =
    if (gameState.currentAreaInitialized)
      gameState.creatures.keys.toList.foldMap(handleCreaturePhysicsUpdate)
    else
      Monoid[GameStateTransition].empty

  private def updateCreatures(delta: Float)(implicit gameState: GameState): GameStateTransition =
    if (gameState.currentAreaInitialized)
      gameState.creatures.keys.toList.foldMap(implicit id => getCreature.update(delta) |+| processCreaturePathfinding())
    else
      Monoid[GameStateTransition].empty

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
        val path =
          Astar.findPath(terrain, getCreature.state.pos, target.state.pos, getCreature.capability)

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

      } else
        State[GameState, List[ExternalEvent]] { implicit gameState =>
          (modifyCreature(_.modify(_.state.pathTowardsTarget).setTo(None)), List())
        }
    } else
      Monoid[GameStateTransition].empty
  }

  def mousePosWindowScaled: Vec2 = {
    val v = new Vector3(Gdx.input.getX.toFloat, Gdx.input.getY.toFloat, 0f)
    hudCamera.unproject(v)
    Vec2(v.x, v.y)
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
