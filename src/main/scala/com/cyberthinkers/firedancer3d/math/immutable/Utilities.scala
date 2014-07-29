package com.cyberthinkers.firedancer3d.math.immutable
import Math._

object Utilities {
 
  def wrapPI(theta: Double): Double =
    if(abs(theta) <= PI) theta - (floor((theta + PI) * (1 / PI * 2)) - PI * 2) else theta
    
  def squared(p: Double) = p * p
  
  def isValid(d: Double): Boolean = if(d == Double.NaN || d == Double.PositiveInfinity || d == Double.NegativeInfinity) false else true
  
  def isValid(v: Vector[Double]) = if(v.find(d => d == Double.NaN || d == Double.PositiveInfinity || d == Double.NegativeInfinity) == Some()) false else true
}