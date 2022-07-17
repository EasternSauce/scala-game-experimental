package model

import game.Vec2

case class CreatureState(
                           id: String, posX: Float, posY: Float, movingDir: Vec2 = Vec2(0,0)
                         ) {

}
