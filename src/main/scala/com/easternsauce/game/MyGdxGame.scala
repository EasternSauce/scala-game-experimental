package com.easternsauce.game

import com.badlogic.gdx.Game
import com.badlogic.gdx.graphics.g2d.TextureAtlas
import com.badlogic.gdx.maps.tiled.{TiledMap, TmxMapLoader}
import com.easternsauce.model.ids.AreaId

object MyGdxGame extends Game {

  var worldDrawingLayer: DrawingLayer = _
  var hudDrawingLayer: DrawingLayer = _

  var atlas: TextureAtlas = _

  val mapsToLoad =
    Map(
      AreaId("area1") -> "assets/areas/area1",
      AreaId("area2") -> "assets/areas/area2",
      AreaId("area3") -> "assets/areas/area3"
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
    PlayScreen.init(atlas)

    setScreen(PlayScreen)
  }
}
