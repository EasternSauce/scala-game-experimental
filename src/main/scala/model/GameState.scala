package model

import cats.data.State
import cats.implicits.toTraverseOps
import com.softwaremill.quicklens._

case class GameState(creatures: Map[String, Creature] = Map()) {}

object GameState {
  def creatureLens(creatureId: String)(implicit gameState: GameState): PathModify[GameState, Creature] =
    modify(gameState)(_.creatures.at(creatureId))

  def updateCreatures()(implicit gameState: GameState): State[GameState, Unit] = {
    gameState.creatures.map { case (_, creature) => creature.update() }.toList.sequence.map(_ => ())
  }

}
