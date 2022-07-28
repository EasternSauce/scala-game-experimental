package com.easternsauce.model.ability

import com.easternsauce.model.Vec2

case class Attack(dirVector: Vec2 = Vec2(1, 0), hitbox: Hitbox)
