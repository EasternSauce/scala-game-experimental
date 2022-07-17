package model

import cats.data.State
import com.softwaremill.quicklens._
import game.ExternalEvent
import model.GameState.{creatureLens, creatureState}

case class Player(state: CreatureState) extends Creature {
  def update(): State[GameState, List[ExternalEvent]] =
    State { implicit gameState =>
      (
        creatureLens(state.id).using(
          _.modify(_.posX)
            .using(_ + 0.0005f * creatureState(state.id).movingDir.x)
            .modify(_.posY)
            .using(_ + 0.0005f * creatureState(state.id).movingDir.y)
        ),
        List()
      )

    }

  override def copy(state: CreatureState = state): Creature = Player(state)
}
