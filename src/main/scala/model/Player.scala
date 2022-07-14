package model

import cats.data.State
import com.softwaremill.quicklens._
import model.GameState.creatureLens

case class Player(id: String, posX: Float, posY: Float) extends Creature {
  def update(): State[GameState, Unit] =
    State.modify { implicit gameState =>
      creatureLens(id).using(
        _.modify(_.posX)
          .using(_ + 0.0005f)
          .modify(_.posY)
          .using(_ + 0.0005f)
      )

    }

  override def copy(id: String = id, posX: Float = posX, posY: Float = posY): Creature = Player(id, posX, posY)
}
