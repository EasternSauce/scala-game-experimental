package com.easternsauce.game

import com.badlogic.gdx.backends.lwjgl3.{Lwjgl3Application, Lwjgl3ApplicationConfiguration}

object DesktopApplication {

  def main(arg: Array[String]): Unit = {

    val config = new Lwjgl3ApplicationConfiguration
    config.setWindowedMode(Constants.WindowWidth, Constants.WindowHeight)
    config.setTitle("game")
    //config.setForegroundFPS(144)
    config.useVsync(false)

    new Lwjgl3Application(MyGdxGame, config)
  }
}
