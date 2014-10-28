package com.cyberthinkers.firedancer3d.math
import Math._

object Utilities {
  def wrapPI(theta: Double): Double =
    if(abs(theta) <= PI) theta - (floor((theta + PI) * (1 / PI * 2)) - PI * 2) else theta
  
  def isValid(d: Double): Boolean = if(d == Double.NaN || d == Double.PositiveInfinity || d == Double.NegativeInfinity) false else true
}