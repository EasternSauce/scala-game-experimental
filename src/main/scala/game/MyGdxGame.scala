package game

import com.badlogic.gdx.Game
import com.badlogic.gdx.graphics.g2d.{SpriteBatch, TextureAtlas}
import com.badlogic.gdx.maps.tiled.{TiledMap, TmxMapLoader}

object MyGdxGame extends Game {

  var spriteBatch: SpriteBatch = _

  var atlas: TextureAtlas = _

  val mapsToLoad =
    Map("area1" -> "assets/areas/area1", "area2" -> "assets/areas/area2", "area3" -> "assets/areas/area3")

  var maps: Map[String, TiledMap] = _

  var mapLoader: TmxMapLoader = _

  override def create(): Unit = {

    spriteBatch = new SpriteBatch()
    atlas = new TextureAtlas("assets/atlas/packed_atlas.atlas")

    mapLoader = new TmxMapLoader()

    maps = mapsToLoad.map {
      case (areaId, directory) => areaId -> mapLoader.load(directory + "/tile_map.tmx")
    }

    PlayScreen.setSpriteBatch(spriteBatch)
    PlayScreen.setMaps(maps)
    PlayScreen.init()

    setScreen(PlayScreen)
  }
}
