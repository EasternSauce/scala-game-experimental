package com.easternsauce.model.area

import com.easternsauce.model.ids.{AreaId, CreatureId}

case class AreaState(id: AreaId, creatures: List[CreatureId] = List())
