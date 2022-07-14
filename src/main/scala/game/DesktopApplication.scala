package game

import com.badlogic.gdx.backends.lwjgl3.{Lwjgl3Application, Lwjgl3ApplicationConfiguration}

object DesktopApplication {

  def main(arg: Array[String]): Unit = {

    val config = new Lwjgl3ApplicationConfiguration
    config.setWindowedMode(1360, 720)
    config.setTitle("game")
    //config.setForegroundFPS(144)
    config.useVsync(false)

    new Lwjgl3Application(new MyGdxGame, config)
  }
}
