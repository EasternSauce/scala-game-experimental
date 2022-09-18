package com.easternsauce.game

import com.badlogic.gdx.Game
import com.badlogic.gdx.graphics.g2d.TextureAtlas
import com.badlogic.gdx.maps.tiled.{TiledMap, TmxMapLoader}
import com.easternsauce.game.physics.{AreaGateBody, PhysicsEngineController}
import com.easternsauce.model.ids.AreaId

object MyGdxGame extends Game {

  var worldDrawingLayer: DrawingLayer = _
  var hudDrawingLayer: DrawingLayer = _

  var atlas: TextureAtlas = _

  val mapsToLoad: Map[AreaId, String] =
    Map(
      AreaId("area1") -> "assets/areas/area1",
      AreaId("area2") -> "assets/areas/area2",
      AreaId("area3") -> "assets/areas/area3"
    )

  val areaGates: List[AreaGateBody] = List( // TODO: load this from file?
    AreaGateBody(AreaId("area1"), 199.5f, 15f, AreaId("area3"), 17f, 2.5f),
    AreaGateBody(AreaId("area1"), 2f, 63f, AreaId("area2"), 58f, 9f)
  )

  var maps: Map[AreaId, TiledMap] = _

  var mapLoader: TmxMapLoader = _

  override def create(): Unit = {
    Assets.loadAssets()

    worldDrawingLayer = DrawingLayer()
    hudDrawingLayer = DrawingLayer()

    atlas = new TextureAtlas("assets/atlas/packed_atlas.atlas")

    mapLoader = new TmxMapLoader()

    maps = mapsToLoad.map {
      case (areaId, directory) => areaId -> mapLoader.load(directory + "/tile_map.tmx")
    }

    PlayScreen.setWorldDrawingLayer(worldDrawingLayer)
    PlayScreen.setHudDrawingLayer(hudDrawingLayer)
    PlayScreen.setMaps(maps)
    PlayScreen.setAreaGates(areaGates)
    PlayScreen.init(atlas)

    areaGates.foreach(_.init(PhysicsEngineController.physicsWorlds))

    setScreen(PlayScreen)
  }
}
