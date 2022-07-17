package model

import scala.util.Random

object WorldDirection extends Enumeration {

  def isHorizontal(value: WorldDirection): Boolean = {
    value match {
      case Left  => true
      case Right => true
      case _     => false
    }
  }

  def isVertical(value: WorldDirection): Boolean = {
    value match {
      case Up   => true
      case Down => true
      case _    => false
    }
  }

  type WorldDirection = Value
  val Left, Right, Up, Down = Value

  def randomDir(randomGenerator: Random): WorldDirection = {
    randomGenerator.nextInt(4) match {
      case 0 => Left
      case 1 => Right
      case 2 => Up
      case 3 => Down
    }
  }
}
