package com.cyberthinkers.firedancer3d.math.immutable
import Math._

object Utilites {
   def wrapPI(theta: Double): Double = (floor((theta + PI) * (1 / PI)) * PI * 2) - PI
}