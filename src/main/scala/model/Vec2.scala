package model

import com.badlogic.gdx.math.Vector2

case class Vec2(x: Float, y: Float) {

  val vector2: Vector2 = new Vector2(x, y)

  def normal: Vec2 = {
    val v2 = vector2.cpy().nor()
    Vec2(v2.x, v2.y)
  }

  def rotate(degrees: Float): Vec2 = {
    val v2 = vector2.cpy().rotateDeg(degrees)
    Vec2(v2.x, v2.y)
  }

  def angleDeg(): Float = {
    vector2.angleDeg()
  }

  def distance(point: Vec2): Float = {
    vector2.dst(point.vector2)
  }

  def vectorTowards(point: Vec2): Vec2 = {
    Vec2(point.x - x, point.y - y).normal
  }

  def length: Float = vector2.len()
}
