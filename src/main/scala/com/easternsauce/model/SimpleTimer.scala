package com.easternsauce.model

import com.softwaremill.quicklens.ModifyPimp

case class SimpleTimer private (time: Float = 0f, isRunning: Boolean = false) {
  def start(): SimpleTimer = this.modify(_.isRunning).setTo(true)

  def stop(): SimpleTimer = this.modify(_.time).setTo(0f).modify(_.isRunning).setTo(false)

  def restart(): SimpleTimer = this.modify(_.time).setTo(0f).modify(_.isRunning).setTo(true)

  def update(delta: Float): SimpleTimer = this.modify(_.time).usingIf(isRunning)(_ + delta)

}
