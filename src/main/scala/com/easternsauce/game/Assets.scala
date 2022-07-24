package com.easternsauce.game

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.assets.AssetManager
import com.badlogic.gdx.audio.{Music, Sound}
import com.badlogic.gdx.graphics.Color
import com.badlogic.gdx.graphics.Texture.TextureFilter
import com.badlogic.gdx.graphics.g2d.freetype.FreeTypeFontGenerator
import com.badlogic.gdx.graphics.g2d.{Batch, BitmapFont, TextureAtlas}

import scala.language.implicitConversions

object Assets {
  val area1DataLocation = "assets/areas/area1"
  val area2DataLocation = "assets/areas/area2"
  val area3DataLocation = "assets/areas/area3"

  var defaultFont: BitmapFont = _
  var hugeFont: BitmapFont = _

  implicit def bitmapFontToEnrichedBitmapFont(font: BitmapFont): EnrichedBitmapFont = new EnrichedBitmapFont(font)

  private val soundsPaths = Map(
    "attack" -> "assets/sounds/swoosh.wav",
    "pain" -> "assets/sounds/pain.wav",
    "arrowWhizz" -> "assets/sounds/arrow-whizz.wav",
    "bloodSquirt" -> "assets/sounds/blood-squirt.wav",
    "boneClick" -> "assets/sounds/bone-click.wav",
    "boneCrush" -> "assets/sounds/bone-crush.wav",
    "bowPull" -> "assets/sounds/bow-pull.wav",
    "bowRelease" -> "assets/sounds/bow-release.wav",
    "darkLaugh" -> "assets/sounds/dark-laugh.wav",
    "dogBark" -> "assets/sounds/dogbark.wav",
    "dogWhine" -> "assets/sounds/dogwhine.wav",
    "evilYelling" -> "assets/sounds/evil-yelling.wav",
    "explosion" -> "assets/sounds/explosion.wav",
    "flyby" -> "assets/sounds/flyby.wav",
    "glassBreak" -> "assets/sounds/glass-break.wav",
    "grunt" -> "assets/sounds/grunt.wav",
    "monsterGrowl" -> "assets/sounds/monster-growl.wav",
    "punch" -> "assets/sounds/punch.wav",
    "roar" -> "assets/sounds/roar.wav",
    "running" -> "assets/sounds/running.wav",
    "strongPunch" -> "assets/sounds/strong-punch.wav",
    "swoosh" -> "assets/sounds/swoosh.wav",
    "chestOpening" -> "assets/sounds/chest-opening.wav",
    "coinBag" -> "assets/sounds/coinbag.wav",
    "matchIgnite" -> "assets/sounds/match-ignite.wav",
    "appleCrunch" -> "assets/sounds/apple-crunch.wav",
    "boneRattle" -> "assets/sounds/bone-rattle.wav"
  )

  private val musicPaths =
    Map("abandonedPlains" -> "assets/music/abandoned_plains.wav", "fireDemon" -> "assets/music/fire_demon.wav")

  val youngSerifFontPath = "assets/font/YoungSerif-Regular.ttf"

  private var assetManager: AssetManager = _
  var atlas: TextureAtlas = _

  def loadAssets(): Unit = {
    atlas = new TextureAtlas("assets/atlas/packed_atlas.atlas")

    assetManager = new AssetManager()

    Assets.soundsPaths.values.foreach(assetManager.load(_, classOf[Sound]))
    Assets.musicPaths.values.foreach(assetManager.load(_, classOf[Music]))

    assetManager.finishLoading()

    def loadFont(assetPath: String, size: Int): BitmapFont = {
      val generator = new FreeTypeFontGenerator(Gdx.files.internal(assetPath))
      val parameter = new FreeTypeFontGenerator.FreeTypeFontParameter
      parameter.size = size
      val font: BitmapFont = generator.generateFont(parameter)
      font.getRegion.getTexture.setFilter(TextureFilter.Linear, TextureFilter.Linear)

      generator.dispose()
      font
    }

    defaultFont = loadFont(Assets.youngSerifFontPath, 16)
    hugeFont = loadFont(Assets.youngSerifFontPath, 64)
  }

  def sound(soundName: String): Sound = {
    assetManager.get(soundsPaths(soundName), classOf[Sound])
  }

  def music(musicName: String): Music = {
    assetManager.get(musicPaths(musicName), classOf[Music])
  }

  class EnrichedBitmapFont(val font: BitmapFont) {
    def draw(batch: Batch, str: CharSequence, x: Float, y: Float, color: Color): Unit = {
      font.setColor(color)
      font.draw(batch, str, x, y)
    }
  }

}
